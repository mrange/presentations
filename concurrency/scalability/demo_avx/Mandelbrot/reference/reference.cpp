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

namespace
{
  std::size_t iterations = 0;

  auto mandelbrot (double cx, double cy)
  {
    auto x    = cx      ;
    auto y    = cy      ;
    auto iter = max_iter;

    for (; iter > 0; --iter)
    {
      auto x2 = x*x;
      auto y2 = y*y;
      if (x2 + y2 > 4)
      {
        iterations += max_iter - iter + 1;
        return iter;
      }
      y = 2*x*y   + cy  ;
      x = x2 - y2 + cx  ;
    }

    iterations += max_iter;
    return iter;
  }

  std::vector<std::uint8_t> compute_set (std::size_t const dim)
  {
    std::vector<std::uint8_t> set;

    auto width = (dim - 1) / 8 + 1;

    set.reserve (width*dim);

    auto scalex = (max_x - min_x) / dim;
    auto scaley = (max_y - min_y) / dim;

    for (auto y = 0U; y < dim; ++y)
    {
      for (auto w = 0U; w < width; ++w)
      {
        std::uint8_t bits = 0;
        for (auto bit = 0U; bit < 8U; ++bit)
        {
          auto x = w*8 + bit;

          auto i = mandelbrot (scalex*x + min_x, scaley*y + min_y);

          if (i == 0)
          {
            bits |= 1 << (7U - bit);
          }
        }
        set.push_back (bits);
      }
    }

    return set;
  }
}

int main (int argc, char const * argv[])
{
  auto result = do_main ("mandelbrot_reference.pbm", argc, argv, &compute_set);

  std::printf ("  iterations: %zu", iterations);

  return result;
}

