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
  auto time_it (T && a)
  {
    auto before = std::chrono::high_resolution_clock::now ();
    auto result = a ();
    auto after  = std::chrono::high_resolution_clock::now ();
    auto diff   = std::chrono::duration_cast<std::chrono::milliseconds> (after - before).count ();
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

    std::printf ("  it took %lld ms\n", ms);

    auto file = std::fopen (pbm_name, "wb");

    std::fprintf (file, "P4\n%d %d\n", dim, dim);
    std::fwrite (&set.front (), 1, set.size (), file);

    std::fclose (file);

    return 0;
  }
}
