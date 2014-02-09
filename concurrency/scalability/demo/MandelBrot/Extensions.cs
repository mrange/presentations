// ----------------------------------------------------------------------------------------------
// Copyright (c) Mårten Rånge.
// ----------------------------------------------------------------------------------------------
// This source code is subject to terms and conditions of the Microsoft Public License. A 
// copy of the license can be found in the License.html file at the root of this distribution. 
// If you cannot locate the  Microsoft Public License, please send an email to 
// dlr@microsoft.com. By using this source code in any fashion, you are agreeing to be bound 
//  by the terms of the Microsoft Public License.
// ----------------------------------------------------------------------------------------------
// You must not remove this notice, or any other, from this software.
// ----------------------------------------------------------------------------------------------
using System;
using System.Windows;
using System.Windows.Media;

namespace MandelBrot
{
    struct Vector4
    {
        public double X;
        public double Y;
        public double Z;
        public double W;

        public static Vector4 operator+(Vector4 left, Vector4 right)
        {
            var result = left;
            result.X += right.X;
            result.Y += right.Y;
            result.Z += right.Z;
            result.W += right.W;
            return result;
        }

        public static Vector4 operator-(Vector4 left, Vector4 right)
        {
            var result = left;
            result.X -= right.X;
            result.Y -= right.Y;
            result.Z -= right.Z;
            result.W -= right.W;
            return result;
        }

        public static Vector4 operator*(Vector4 left, Vector4 right)
        {
            var result = left;
            result.X *= right.X;
            result.Y *= right.Y;
            result.Z *= right.Z;
            result.W *= right.W;
            return result;
        }

        public static Vector4 operator*(Vector4 left, double scale)
        {
            var result = left;
            result.X *= scale;
            result.Y *= scale;
            result.Z *= scale;
            result.W *= scale;
            return result;
        }

        public static Vector4 Create (double x, double y, double z, double w)
        {
            return new Vector4
            {
                X = x,
                Y = y,
                Z = z,
                W = w,
            };
        }

    }

    static class Extensions
    {


        public static Vector4 ToVector4 (this Color c)
        {
            const double multiplier = 1 / 255.0;
            return Vector4.Create(
                multiplier * c.A,
                multiplier * c.R,
                multiplier * c.G,
                multiplier * c.B
                );
        }

        public static Color ToColor (this Vector4 v)
        {
            const double multiplier = 255.0;
            return Color.FromArgb(
                (byte) Math.Round(multiplier * v.X),
                (byte) Math.Round(multiplier * v.Y),
                (byte) Math.Round(multiplier * v.Z),
                (byte) Math.Round(multiplier * v.W)
                );
        }

        public static Vector4 Lerp(this double t, Vector4 from, Vector4 to)
        {
            var diff = to - from;
            return from + diff*t;
        }

    }
}
