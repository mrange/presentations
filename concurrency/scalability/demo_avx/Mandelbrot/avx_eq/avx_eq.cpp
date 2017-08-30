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
# include <intrin.h>  
#endif

namespace
{
  void mandelbrot (
      std::size_t     sz
    , float const *   cxs
    , float           cy0
    , std::uint8_t *  pixels
    )
  {
    assert(sz > 7);
    assert(sz % 8 == 0);

    auto cx     = _mm256_loadu_ps (cxs);
    auto cy     = _mm256_set1_ps (cy0);

    auto x      = cx;
    auto y      = cy;
    auto sub    = _mm256_set1_ps (1.0);
    auto rem    = _mm256_set1_ps (max_iter - 1);

    auto i      = 8;
    int slots[] = {0, 1, 2, 3, 4, 5, 6, 7};

    auto active = 8;

    auto updater = [&](int mask, auto && action)
    {
        while (mask)
        {
          auto next   = mask & (mask - 1);
          auto slot   = 0UL;
          _BitScanForward(&slot, mask);
          action(slots[slot]);
          if (i < sz)
          {
            slots[slot]         = i;
            rem.m256_f32[slot]  = max_iter - 1;
            cx.m256_f32[slot]   = cxs[i];
            x.m256_f32[slot]    = cxs[i];
            y.m256_f32[slot]    = cy0;
            ++i;
          }
          else
          {
            slots[slot]         = -1 ;
            rem.m256_f32[slot]  = 0;
            sub.m256_f32[slot]  = 0;
            cx.m256_f32[slot]   = 0;
            cy.m256_f32[slot]   = 0;
            x.m256_f32[slot]    = 0;
            y.m256_f32[slot]    = 0;
            --active;
          }

          mask = next;
        }
    };

    do
    {
      auto x2         = _mm256_mul_ps  (x, x);
      auto y2         = _mm256_mul_ps  (y, y);
      auto r2         = _mm256_add_ps  (x2, y2);

      {
        auto inf_mask   = _mm256_movemask_ps (_mm256_cmp_ps  (r2, _mm256_set1_ps (4.0), _CMP_NLT_UQ));
        if (inf_mask)
        {
          updater(inf_mask, [](auto &&) {});
          x2 = _mm256_mul_ps  (x, x);
          y2 = _mm256_mul_ps  (y, y);
        }
      }

      auto xy       = _mm256_mul_ps (x, y);
      y             = _mm256_add_ps (_mm256_add_ps (xy, xy) , cy);
      x             = _mm256_add_ps (_mm256_sub_ps (x2, y2) , cx);

      {
        rem           = _mm256_sub_ps (rem, sub);
        auto rem_mask = _mm256_movemask_ps (rem);
        updater(rem_mask, [&](auto && p)
          {
            auto bit    = (7 - p % 8);
            auto idx    = p / 8;
            pixels[idx] |=(1 << bit);
          });
      }

    } while(active);
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

    std::memset (pset, 0, set->w*set->y);

    std::vector<float> cxs;
    cxs.resize (dim);
    {
      auto x = min_x;
      for (auto iter = 0U; iter < dim; ++iter)
      {
        cxs[iter] = x;
        x += scalex;
      }
    }

    auto sdim   = static_cast<int> (dim);

    #pragma omp parallel for schedule(guided)
    for (auto sy = 0; sy < sdim; ++sy)
    {
      auto y        = static_cast<std::size_t> (sy);
      auto yoffset  = y*width;

      mandelbrot (dim, &cxs.front (), scaley*y + min_y, pset + yoffset);
    }

    return set;
  }
}

int main (int argc, char const * argv[])
{
  return do_main ("mandelbrot_avx_eq.pbm", argc, argv, &compute_set);
}

