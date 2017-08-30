// ----------------------------------------------------------------------------------------------
// Copyright 2017 M�rten R�nge
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
// ----------------------------------------------------------------------------------------------

#include "stdafx.h"

#include <vector>

#include "../common.hpp"

#ifdef _MSVC_LANG
# pragma warning(disable : 4459)
#endif

#define MANDEL_COMPUTE(i)                                             \
        xy[i] = _mm256_mul_ps (x[i], y[i]);                           \
        x2[i] = _mm256_mul_ps (x[i], x[i]);                           \
        y2[i] = _mm256_mul_ps (y[i], y[i]);                           \
        y[i] = _mm256_add_ps (_mm256_add_ps (xy[i], xy[i]), cy[i]);   \
        x[i] = _mm256_add_ps (_mm256_sub_ps (x2[i], y2[i]), cx[i]);

#define MANDEL_ITERATION()  \
  MANDEL_COMPUTE(0)     \
  MANDEL_COMPUTE(1)     \
  MANDEL_COMPUTE(2)     \
  MANDEL_COMPUTE(3)

#define MANDEL_CMP(i) \
  _mm256_cmp_ps (_mm256_add_ps (x2[i], y2[i]), _mm256_set1_ps (4.0F), _CMP_LE_OQ)

#define MANDEL_CMPMASK()                          \
  std::uint32_t cmp_mask =                        \
      (_mm256_movemask_ps (MANDEL_CMP(0))      )  \
    | (_mm256_movemask_ps (MANDEL_CMP(1)) << 8 )  \
    | (_mm256_movemask_ps (MANDEL_CMP(2)) << 16)  \
    | (_mm256_movemask_ps (MANDEL_CMP(3)) << 24)

#define MANDEL_CHECKINF()                         \
  auto cont = _mm256_movemask_ps (_mm256_or_ps (  \
      _mm256_or_ps (MANDEL_CMP(0), MANDEL_CMP(1)) \
    , _mm256_or_ps (MANDEL_CMP(2), MANDEL_CMP(3)) \
    ));                                           \
  if (!cont)                                      \
  {                                               \
    return 0;                                     \
  }

namespace
{
  MANDEL_INLINE int mandelbrot_avx (__m256 cx[4], __m256 cy[4])
  {

    __m256  x[4] {cx[0], cx[1], cx[2], cx[3]};
    __m256  y[4] {cy[0], cy[1], cy[2], cy[3]};
    __m256 x2[4];
    __m256 y2[4];
    __m256 xy[4];

    // 6 * 8 + 2 => 50 iterations
    for (auto iter = 6; iter > 0; --iter)
    {
      // 8 inner steps
      MANDEL_ITERATION();
      MANDEL_ITERATION();
      MANDEL_ITERATION();
      MANDEL_ITERATION();
      MANDEL_ITERATION();
      MANDEL_ITERATION();
      MANDEL_ITERATION();
      MANDEL_ITERATION();

      MANDEL_CHECKINF();
    }

    // Last 2 steps
    MANDEL_ITERATION();
    MANDEL_ITERATION();

    MANDEL_CMPMASK();

    return cmp_mask;
  }

  MANDEL_INLINE int mandelbrot_avx_full (__m256 cx[4], __m256 cy[4])
  {

    __m256  x[4] {cx[0], cx[1], cx[2], cx[3]};
    __m256  y[4] {cy[0], cy[1], cy[2], cy[3]};
    __m256 x2[4];
    __m256 y2[4];
    __m256 xy[4];

    // 6 * 8 + 2 => 50 iterations
    for (auto iter = 6; iter > 0; --iter)
    {
      // 8 inner steps
      MANDEL_ITERATION();
      MANDEL_ITERATION();
      MANDEL_ITERATION();
      MANDEL_ITERATION();
      MANDEL_ITERATION();
      MANDEL_ITERATION();
      MANDEL_ITERATION();
      MANDEL_ITERATION();
    }

    // Last 2 steps
    MANDEL_ITERATION();
    MANDEL_ITERATION();

    MANDEL_CMPMASK();

    return cmp_mask;
  }

  bitmap::uptr compute_set (std::size_t const dim)
  {
    auto set    = create_bitmap (dim, dim);
    auto width  = set->w;
    auto pset   = set->bits ();

    auto max_x  = static_cast<float> (::max_x);
    auto min_x  = static_cast<float> (::min_x);

    auto max_y  = static_cast<float> (::max_y);
    auto min_y  = static_cast<float> (::min_y);

    auto scalex = (max_x - min_x) / dim;
    auto scaley = (max_y - min_y) / dim;

    auto incx   = _mm256_set_ps (
        0*scalex
      , 1*scalex
      , 2*scalex
      , 3*scalex
      , 4*scalex
      , 5*scalex
      , 6*scalex
      , 7*scalex
      );

    auto incy1  = _mm256_set1_ps (1*scaley);
    auto incy2  = _mm256_set1_ps (2*scaley);
    auto incy3  = _mm256_set1_ps (3*scaley);

    auto sdim   = static_cast<int> (dim);

    #pragma omp parallel for schedule(guided)
    for (auto sy = 0; sy < sdim; sy += 4)
    {
      auto y        = static_cast<std::size_t> (sy);
      auto yoffset  = y*width;

      auto last_reached_full  = false;

      for (auto w = 0U; w < width; ++w)
      {
        auto x = w << 3;

        __m256 cx_ = _mm256_add_ps  (_mm256_set1_ps (scalex*x + min_x), incx);
        __m256 cy_ = _mm256_set1_ps (scaley*y + min_y);

        __m256 cx[4] = { cx_, cx_                        , cx_                       , cx_                       };
        __m256 cy[4] = { cy_, _mm256_add_ps (cy_, incy1) , _mm256_add_ps (cy_, incy2), _mm256_add_ps (cy_, incy3)};

        auto bits = 
          last_reached_full
            ? mandelbrot_avx_full (cx, cy)
            : mandelbrot_avx (cx, cy)
            ;

        pset[yoffset            + w] = static_cast<std::uint8_t> (bits      );
        pset[yoffset + 1*width  + w] = static_cast<std::uint8_t> (bits >> 8 );
        pset[yoffset + 2*width  + w] = static_cast<std::uint8_t> (bits >> 16);
        pset[yoffset + 3*width  + w] = static_cast<std::uint8_t> (bits >> 24);

        last_reached_full = bits != 0;
      }
    }

    return set;
  }
}

int main (int argc, char const * argv[])
{
  return do_main ("mandelbrot_unroll.pbm", argc, argv, &compute_set);
}

