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

#include <cstddef>
#include <cstdio>
#include <chrono>
#include <vector>
#include <tuple>

namespace
{
  constexpr auto    min_x    = -1.5 ;
  constexpr auto    min_y    = -1.0 ;
  constexpr auto    max_x    =  0.5 ;
  constexpr auto    max_y    =  1.0 ;
  constexpr auto    max_iter =  50U ;

  template<typename T>
  auto time_it (T a)
  {
    auto before = std::chrono::high_resolution_clock::now ();
    auto result = a ();
    auto after  = std::chrono::high_resolution_clock::now ();
    auto diff   = std::chrono::duration_cast<std::chrono::milliseconds> (after - before).count ();
    return std::make_tuple (diff, std::move (result));
  }

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
        return iter;
      }
      x = x2 - y2 + cx  ;
      y = 2*x*y   + cy  ;
    }

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

    std::printf ("  it took %lld ms\n", ms);

    auto file = std::fopen (pbm_name, "wb");

    std::fprintf (file, "P4\n%d %d\n", dim, dim);
    std::fwrite (&set.front (), 1, set.size (), file);

    std::fclose (file);

    return 0;
  }

}

int main (int argc, char const * argv[])
{
  return do_main ("mandelbrot_reference.pbm", argc, argv, &compute_set);
}

