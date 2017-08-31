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

#define MANDEL_CMPMASK(i)         \
  x2[i] = x[i]*x[i];              \
  y2[i] = y[i]*y[i];              \
  cm[i] = x2[i] + y2[i] <= _4;

#define MANDEL_NEXT(i)            \
  xy[i] = x[i]*y[i];              \
  x [i] = x2[i] - y2[i] + cx[i];  \
  y [i] = xy[i] + xy[i] + cy[i];


namespace
{
  constexpr auto simultaneous = 4;

  MANDEL_INLINE auto mandelbrot (__m256 cx[simultaneous], __m256 cy[simultaneous])
  {
    __m256 x[simultaneous] = { cx[0], cx[1], cx[2], cx[3] };
    __m256 y[simultaneous] = { cy[0], cy[1], cy[2], cy[3] };
    int   cm[simultaneous];

    for (auto iter = max_iter; iter > 0; --iter)
    {
      auto _4         = float8 (4.0F);

      __m256 x2[simultaneous];
      __m256 y2[simultaneous];
      __m256 xy[simultaneous];

      MANDEL_CMPMASK (0);
      MANDEL_CMPMASK (1);
      MANDEL_CMPMASK (2);
      MANDEL_CMPMASK (3);

      auto cmp_mask =
            cm[0]
          | cm[1]
          | cm[2]
          | cm[3]
          ;

      if (!cmp_mask)
      {
        return 0;
      }

      MANDEL_NEXT (0);
      MANDEL_NEXT (1);
      MANDEL_NEXT (2);
      MANDEL_NEXT (3);

    }

    auto cmp_mask =
          (cm[0]        )
        | (cm[1] << 8   )
        | (cm[2] << 16  )
        | (cm[3] << 24  )
        ;

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

    auto incx   = float8 (
        0*scalex
      , 1*scalex
      , 2*scalex
      , 3*scalex
      , 4*scalex
      , 5*scalex
      , 6*scalex
      , 7*scalex
      );

    auto incy1  = float8 (1*scaley);
    auto incy2  = float8 (2*scaley);
    auto incy3  = float8 (3*scaley);

    auto sdim   = static_cast<int> (dim);

    #pragma omp parallel for schedule(guided)
    for (auto sy = 0; sy < sdim; sy += 4)
    {
      auto y        = static_cast<std::size_t> (sy);
      auto yoffset  = y*width;

      for (auto w = 0U; w < width; ++w)
      {
        auto x = w << 3;

        __m256 cx_ = float8 (scalex*x + min_x) + incx;
        __m256 cy_ = float8 (scaley*y + min_y);

        __m256 cx[simultaneous] = { cx_, cx_          , cx_         , cx_        };
        __m256 cy[simultaneous] = { cy_, cy_ + incy1  , cy_ + incy2 , cy_ + incy3};

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
  return do_main ("mandelbrot_avx4.pbm", argc, argv, &compute_set);
}

