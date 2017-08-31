// ----------------------------------------------------------------------------------------------
// Copyright 2017 Mårten Rånge
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

#include <cassert>
#include <cstddef>
#include <cstdio>
#include <chrono>
#include <memory>
#include <vector>
#include <tuple>

//#include <intrin.h>
#include <emmintrin.h>
#include <immintrin.h>

#ifdef _MSVC_LANG
# define MANDEL_INLINE __forceinline
#else
# define MANDEL_INLINE inline
#endif

namespace
{
  constexpr auto    min_x    = -1.5 ;
  constexpr auto    min_y    = -1.0 ;
  constexpr auto    max_x    =  0.5 ;
  constexpr auto    max_y    =  1.0 ;
  constexpr auto    max_iter =  50U ;

  template<typename T>
  auto time_it (T && a)
  {
    auto before = std::chrono::high_resolution_clock::now ();
    auto result = a ();
    auto after  = std::chrono::high_resolution_clock::now ();
    auto diff   = static_cast<int> (std::chrono::duration_cast<std::chrono::milliseconds> (after - before).count ());
    return std::make_tuple (diff, std::move (result));
  }

  template<typename TGenerator>
  int do_main (char const * pbm_name, int argc, char const * argv[], TGenerator && generator)
  {
    auto dim  = [argc, argv] ()
    {
      auto dim = argc > 1 ? atoi (argv[1]) : 0;
      return dim > 0 ? dim : 200;
    } ();

    if (dim % 8 != 0)
    {
      std::printf ("Dimension must be modulo 8\n");
      return 999;
    }

    std::printf ("Generating mandelbrot set %dx%d(%d)\n", dim, dim, max_iter);

    auto res  = time_it ([dim, &generator] { return generator(dim); });

    auto ms   = std::get<0> (res);
    auto& set = std::get<1> (res);

    std::printf ("  it took %d ms\n", ms);

    auto file = std::fopen (pbm_name, "wb");

    std::fprintf (file, "P4\n%d %d\n", dim, dim);
    std::fwrite (set->bits (), 1, set->sz, file);

    std::fclose (file);

    return 0;
  }

  struct bitmap
  {
    using uptr = std::unique_ptr<bitmap>;

    std::size_t const x ;
    std::size_t const y ;
    std::size_t const w ;
    std::size_t const sz;

    bitmap (std::size_t x, std::size_t y) noexcept
      : x   (x)
      , y   (y)
      , w   ((x + 7) / 8)
      , sz  (w*y)
    {
      b = static_cast<std::uint8_t*> (malloc(sz));
    }

    ~bitmap () noexcept
    {
      free (b);
      b = nullptr;
    }

    bitmap (bitmap && bm) noexcept
      : x   (bm.x)
      , y   (bm.y)
      , w   (bm.w)
      , sz  (bm.sz)
      , b   (bm.b)
    {
      bm.b = nullptr;
    }

    bitmap (bitmap const &)             = delete;
    bitmap& operator= (bitmap const &)  = delete;
    bitmap& operator= (bitmap &&)       = delete;

    std::uint8_t * bits () noexcept
    {
      assert (b);
      return b;
    }

    std::uint8_t const * bits () const noexcept
    {
      assert (b);
      return b;
    }

  private:
    std::uint8_t * b;
  };

  bitmap::uptr create_bitmap (std::size_t x, std::size_t y)
  {
    return std::make_unique<bitmap> (x, y);
  }

  MANDEL_INLINE __m256 __cdecl operator+ (__m256 l, __m256 r) noexcept
  {
    return _mm256_add_ps (l, r);
  }

  MANDEL_INLINE __m256 __cdecl operator- (__m256 l, __m256 r) noexcept
  {
    return _mm256_sub_ps (l, r);
  }

  MANDEL_INLINE __m256 __cdecl operator* (__m256 l, __m256 r) noexcept
  {
    return _mm256_mul_ps (l, r);
  }

  MANDEL_INLINE int __cdecl operator<= (__m256 l, __m256 r) noexcept
  {
    return _mm256_movemask_ps (_mm256_cmp_ps (l, r, _CMP_LE_OQ));
  }

  MANDEL_INLINE __m256 __cdecl float8 (float v) noexcept
  {
    return _mm256_set1_ps (v);
  }

  MANDEL_INLINE __m256 __cdecl float8 (float v0, float v1, float v2, float v3, float v4, float v5, float v6, float v7) noexcept
  {
    return _mm256_set_ps (v0, v1, v2, v3, v4, v5, v6, v7);
  }
}
