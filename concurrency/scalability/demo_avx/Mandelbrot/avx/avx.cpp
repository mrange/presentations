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

namespace
{
  auto mandelbrot (__m256 cx, __m256 cy)
  {
    auto x        = cx;
    auto y        = cy;

    int cmp_mask  = 0 ;

    for (auto iter = max_iter; iter > 0; --iter)
    {
      auto x2         = x*x;
      auto y2         = y*y;
      auto r2         = x2 + y2;
      auto _4         = float8 (4.0F);
      cmp_mask        = r2 <= _4;

      if (!cmp_mask)
      {
        return 0;
      }

      auto xy       = x*y;
      y             = xy + xy + cy;
      x             = x2 - y2 + cx;
    }

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

    auto sdim   = static_cast<int> (dim);

    #pragma omp parallel for schedule(guided)
    for (auto sy = 0; sy < sdim; ++sy)
    {
      auto y        = static_cast<std::size_t> (sy);
      auto yoffset  = y*width;

      for (auto w = 0U; w < width; ++w)
      {
        auto x = w << 3;

        __m256 cx = float8 (scalex*x + min_x) + incx;
        __m256 cy = float8 (scaley*y + min_y);

        auto bits = mandelbrot (cx, cy);

        pset[yoffset + w] = static_cast<std::uint8_t> (bits);
      }
    }

    return set;
  }
}

int main (int argc, char const * argv[])
{
  return do_main ("mandelbrot_avx.pbm", argc, argv, &compute_set);
}

