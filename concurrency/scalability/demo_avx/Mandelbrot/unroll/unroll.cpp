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

#include "stdafx.h"

#include <vector>

#include "../common.hpp"

#pragma warning(disable : 4459)

#define MANDEL_INDEPENDENT(i)                                     \
  xy[i] = _mm256_mul_ps (x[i], y[i]);                             \
  x2[i] = _mm256_mul_ps (x[i], x[i]);                             \
  y2[i] = _mm256_mul_ps (y[i], y[i]);
#define MANDEL_DEPENDENT(i)                                       \
  y[i]  = _mm256_add_ps (_mm256_add_ps (xy[i], xy[i]) , cy[i]);   \
  x[i]  = _mm256_add_ps (_mm256_sub_ps (x2[i], y2[i]) , cx[i]);

#define MANDEL_ITERATION()  \
  MANDEL_INDEPENDENT(0)     \
  MANDEL_DEPENDENT(0)       \
  MANDEL_INDEPENDENT(1)     \
  MANDEL_DEPENDENT(1)       \
  MANDEL_INDEPENDENT(2)     \
  MANDEL_DEPENDENT(2)       \
  MANDEL_INDEPENDENT(3)     \
  MANDEL_DEPENDENT(3)

#define MANDEL_CMPMASK()  \
  cmp_mask      =   \
      (_mm256_movemask_ps (_mm256_cmp_ps (_mm256_add_ps (x2[0], y2[0]), _mm256_set1_ps (4.0F), _CMP_LT_OQ))      ) \
    | (_mm256_movemask_ps (_mm256_cmp_ps (_mm256_add_ps (x2[1], y2[1]), _mm256_set1_ps (4.0F), _CMP_LT_OQ)) << 8 ) \
    | (_mm256_movemask_ps (_mm256_cmp_ps (_mm256_add_ps (x2[2], y2[2]), _mm256_set1_ps (4.0F), _CMP_LT_OQ)) << 16) \
    | (_mm256_movemask_ps (_mm256_cmp_ps (_mm256_add_ps (x2[3], y2[3]), _mm256_set1_ps (4.0F), _CMP_LT_OQ)) << 24)

namespace
{
  constexpr auto simultaneous = 4;

  auto mandelbrot (__m256 cx[simultaneous], __m256 cy[simultaneous])
  {
    __m256 x [simultaneous] = { cx[0], cx[1], cx[2], cx[3] };
    __m256 y [simultaneous] = { cy[0], cy[1], cy[2], cy[3] };

    __m256 x2[simultaneous];
    __m256 y2[simultaneous];
    __m256 xy[simultaneous];

    int cmp_mask    = 0 ;

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

      MANDEL_CMPMASK();

      if (!cmp_mask)
      {
        return 0;
      }
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

      for (auto w = 0U; w < width; ++w)
      {
        auto x = w << 3;

        __m256 cx_ = _mm256_add_ps  (_mm256_set1_ps (scalex*x + min_x), incx);
        __m256 cy_ = _mm256_set1_ps (scaley*y + min_y);

        __m256 cx[simultaneous] = { cx_, cx_                        , cx_                       , cx_                       };
        __m256 cy[simultaneous] = { cy_, _mm256_add_ps (cy_, incy1) , _mm256_add_ps (cy_, incy2), _mm256_add_ps (cy_, incy3)};

        auto bits = mandelbrot (cx, cy);

        pset[yoffset            + w] = static_cast<std::uint8_t> (bits      );
        pset[yoffset + 1*width  + w] = static_cast<std::uint8_t> (bits >> 8 );
        pset[yoffset + 2*width  + w] = static_cast<std::uint8_t> (bits >> 16);
        pset[yoffset + 3*width  + w] = static_cast<std::uint8_t> (bits >> 24);
      }
    }

    return set;
  }
}

int main (int argc, char const * argv[])
{
  return do_main ("mandelbrot_unroll.pbm", argc, argv, &compute_set);
}

