// ----------------------------------------------------------------------------------------------
// Copyright (c) WCOM AB.
// ----------------------------------------------------------------------------------------------
// This source code is subject to terms and conditions of the Microsoft Public License. A 
// copy of the license can be found in the License.html file at the root of this distribution. 
// If you cannot locate the  Microsoft Public License, please send an email to 
// dlr@microsoft.com. By using this source code in any fashion, you are agreeing to be bound 
//  by the terms of the Microsoft Public License.
// ----------------------------------------------------------------------------------------------
// You must not remove this notice, or any other, from this software.
// ----------------------------------------------------------------------------------------------

#include "stdafx.h"

using namespace concurrency;

namespace
{
#define SIMPLE

#ifdef SIMPLE
    typedef         float        mtype                  ;

    unsigned int    const iter  = 256                   ;
    unsigned int    const width = 1024                  ;
    unsigned int    const height= 1024                  ;

    mtype           const cx    = -1                    ;
    mtype           const cy    = 0                     ;

    mtype           const dx    = 3.0F                  ;
    mtype           const dy    = 3.0F                  ;

    unsigned int    const cpu_n = 20U                   ;
    unsigned int    const amp_n = 20U                   ;
#else
    typedef         float      mtype                   ;

    unsigned int    const iter  = 2048                  ;
    unsigned int    const width = 2048                  ;
    unsigned int    const height= 2048                  ;

    mtype           const cx    = 0.001643721971153     ;
    mtype           const cy    = 0.822467633298876     ;

    mtype           const dx    = 0.00000000010         ;
    mtype           const dy    = 0.00000000010         ;

    unsigned int    const cpu_n = 1U                    ;
    unsigned int    const amp_n = 1U                    ;
#endif

    template<typename TPredicate>
    long long execute_testruns (
            std::size_t test_runs
        ,   TPredicate predicate
        )
    {
        // Warm-up
        predicate ();

        auto then = std::chrono::high_resolution_clock::now ();

        for (auto test_run = 0U; test_run < test_runs; ++test_run)
        {
            predicate ();
        }

        auto now = std::chrono::high_resolution_clock::now ();

        auto diff = now - then;

        auto diff_in_ms = std::chrono::duration_cast<std::chrono::milliseconds>(diff).count ();

        return diff_in_ms;
    }

    inline int mandelbrot (mtype x, mtype y, int iter) restrict(amp, cpu)
    {
        auto ix = x;
        auto iy = y;

        auto i = 0;

        // Zn+1 = Zn^2 + C

        for (; (i < iter) & ((ix * ix + iy * iy) < 4); ++i)
        {
            auto tx = ix * ix - iy * iy + x;
            iy = 2 * ix * iy + y;
            ix = tx;
        }
        return i;
    }

    void compute_mandelbrot (
            std::vector<int> & result
        ,   unsigned int    iter
        ,   unsigned int    width
        ,   unsigned int    height
        ,   mtype           cx
        ,   mtype           cy
        ,   mtype           dx
        ,   mtype           dy
        )
    {
        result.resize (width*height);

        auto tx = dx/width;
        auto mx = cx - dx / 2;
        auto ty = dy/height;
        auto my = cy - dy / 2;

        for (auto iy = 0U; iy < height; ++iy)
        {
            for (auto ix = 0U; ix < width; ++ix)
            {
                auto x = tx * ix + mx;
                auto y = ty * iy + my;
                result[ix + iy*width] = mandelbrot (x,y, iter);
            }
        }
    }

    void parallel_compute_mandelbrot (
            std::vector<int> & result
        ,   unsigned int    iter
        ,   unsigned int    width
        ,   unsigned int    height
        ,   mtype           cx
        ,   mtype           cy
        ,   mtype           dx
        ,   mtype           dy
        )
    {
        result.resize (width*height);

        auto tx = dx/width;
        auto mx = cx - dx / 2;
        auto ty = dy/height;
        auto my = cy - dy / 2;

        parallel_for (
                0U
            ,   width*height
            ,   [&] (unsigned int idx)
                {
                    auto ix = idx % width;
                    auto iy = idx / width;

                    auto x = tx * ix + mx;
                    auto y = ty * iy + my;

                    result[idx] = mandelbrot (x,y, iter);
                });
    }

    void amp_compute_void (
            std::vector<int> & result
        ,   unsigned int    width
        ,   unsigned int    height
        )
    {
        result.resize (width*height);

        extent<2> extent (width, height);

        array_view<int, 2> view (extent, result);
        view.discard_data ();

        parallel_for_each (
                    view.extent
                ,   [=] (index<2> idx) restrict(amp)
                {
                    view[idx] = idx[0]*idx[1];
                });

        view.synchronize ();
    }

    void amp_compute_mandelbrot (
            std::vector<int> & result
        ,   unsigned int    iter
        ,   unsigned int    width
        ,   unsigned int    height
        ,   mtype           cx
        ,   mtype           cy
        ,   mtype           dx
        ,   mtype           dy
        )
    {
        result.resize (width*height);

        auto tx = dx/width;
        auto mx = cx - dx / 2;
        auto ty = dy/height;
        auto my = cy - dy / 2;

        extent<2> extent (width, height);

        array_view<int, 2> view (extent, result);
        view.discard_data ();

        parallel_for_each (
                    view.extent
                ,   [=] (index<2> idx) restrict(amp)
                {
                    auto x = tx * idx[1] + mx;
                    auto y = ty * idx[0] + my;

                    view[idx] = mandelbrot (x,y, iter);
                });

        view.synchronize ();
    }




}

int main()
{
#ifdef _DEBUG
    {
        std::vector<int> trivial_result ;
        std::vector<int> parallel_result;
        std::vector<int> amp_result     ;

        compute_mandelbrot (
                trivial_result
            ,   iter
            ,   width
            ,   height
            ,   cx
            ,   cy
            ,   dx
            ,   dy
            );

        parallel_compute_mandelbrot (
                parallel_result
            ,   iter
            ,   width
            ,   height
            ,   cx
            ,   cy
            ,   dx
            ,   dy
            );

        amp_compute_mandelbrot (
                amp_result
            ,   iter
            ,   width
            ,   height
            ,   cx
            ,   cy
            ,   dx
            ,   dy
            );

        auto expected_size = width*height;

        if (
                expected_size == trivial_result.size () 
            &&  expected_size == parallel_result.size ()
            &&  expected_size == amp_result.size ()     
            )
        {
            for (auto iter = 0U; iter < expected_size; ++iter)
            {
                if (trivial_result[iter] != parallel_result[iter])
                {
                    printf ("Error: Parallel result mismatch@%d!\r\n", iter);
                    return 102;
                }
                if (trivial_result[iter] != amp_result[iter])
                {
                    printf ("Error: Amp result mismatch@%d!\r\n", iter);
                    return 103;
                }
            }
        }
        else
        {
            printf ("Error: Size mismatch!\r\n");
            return 101;
        }

        printf ("Results are all ok!\r\n");
    }

#else
    std::vector<int> result;
    result.resize (width*height);

    auto mandelbrot_result = execute_testruns (
            cpu_n
        ,   [&] ()
        {
            compute_mandelbrot (
                    result
                ,   iter
                ,   width
                ,   height
                ,   cx
                ,   cy
                ,   dx
                ,   dy
                );
        });
    printf ("%d trivial mandelbrot iterations took %d ms\r\n"   , cpu_n, (int)mandelbrot_result             );

    auto parallel_mandelbrot_result = execute_testruns (
            cpu_n
        ,   [&] ()
        {
            parallel_compute_mandelbrot (
                    result
                ,   iter
                ,   width
                ,   height
                ,   cx
                ,   cy
                ,   dx
                ,   dy
                );
        });
    printf ("%d parallel mandelbrot iterations took %d ms\r\n"  , cpu_n, (int)parallel_mandelbrot_result    );

    auto amp_void_result = execute_testruns (
            amp_n
        ,   [&] ()
        {
            amp_compute_void (
                    result
                ,   width
                ,   height
                );
        });
    printf ("%d amp void iterations took %d ms\r\n"             , amp_n, (int)amp_void_result               );

    auto amp_mandelbrot_result = execute_testruns (
            amp_n
        ,   [&] ()
        {
            amp_compute_mandelbrot (
                    result
                ,   iter
                ,   width
                ,   height
                ,   cx
                ,   cy
                ,   dx
                ,   dy
                );
        });
    printf ("%d amp mandelbrot iterations took %d ms\r\n"       , amp_n, (int)amp_mandelbrot_result         );

#endif

    return 0;
}

