﻿// ----------------------------------------------------------------------------------------------
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

open System
open System.Diagnostics
open System.Windows
open System.Windows.Controls
open System.Windows.Media
open System.Windows.Media.Imaging
open System.Windows.Threading
open System.Collections.Concurrent

open RayTracer

[<EntryPoint>]
[<STAThread>]
let main argv = 

    let lights = 
       [|
            LightSource.New (White * 0.75) (Vector3.New 2. 3. 2.) 0.25
            LightSource.New (White * 0.25) (Vector3.New 5. 3. 0.) 0.25
            LightSource.New (White * 0.25) (Vector3.New -5. 6. -2.) 0.25
       |]

    let sphereRadius    = 1.
    let orbiterRadius   = 0.25
    let orbiterCount    = int ((sphereRadius + orbiterRadius) * pi2 / (orbiterRadius * 2.))

    let orbiters = 
        [|
            for c in 0..orbiterCount -> 
                let d = (float c) * pi2 / (float orbiterCount)
                let x = (sphereRadius + orbiterRadius) * cos d
                let z = (sphereRadius + orbiterRadius) * sin d
                Sphere(UniformSurface <| Reflective 0.75 White , Vector3.New x sphereRadius z, orbiterRadius).AsShape
        |]



    let white           = Matte (White * 0.75)
    let blue            = Matte (Blue * 0.75)
    let planeSurface    = SquaresSurface 2. white blue
    let sphereSurface   = GradientCirclesSurface 0.1 white (fun t -> Matte (Black.Lerp Green t))
//    let sphereSurface   = SquaresSurface 0.1 white blue


    let plane   = (Plane (planeSurface, 0., Vector3.New 0. 1. 0.)).AsShape

    let placed = 
        [|
            plane
            Sphere(sphereSurface                            , Vector3.New 0. sphereRadius 0., sphereRadius).AsShape
            Sphere(UniformSurface <| Reflective 0.25 Green  , Vector3.New 2. 0.5 1., 0.5).AsShape
            Sphere(UniformSurface <| Reflective 0.25 Blue   , Vector3.New 3. 0.5 0., 0.5).AsShape
        |]

    let world = [| placed; orbiters |] |> Array.concat

    let eye         = Vector3.New 3. 3. 3.
    let at          = Vector3.New 0. 1. 0.
    let clipDistance= 1.
    let clipUp      = Vector3.New 0. 1. 0.
    let fov         = degree2rad 120.
    let granularity = 1.0

    let window = new Window()
    window.MinWidth <- 640.
    window.MinHeight <- 400.


    use loaded = window.Loaded.Subscribe (fun v -> 
        

        let width   = window.Width  / granularity
        let height  = window.Height / granularity
        let ratio   = width / height

        let wb = new WriteableBitmap(int width, int height, 96., 96., PixelFormats.Bgr32, null)

        let iwidth = wb.PixelWidth
        let iheight = wb.PixelHeight

        let viewPort = ViewPort.New eye at clipUp clipDistance fov ((float iheight) / (float iwidth))

        let queue = ConcurrentQueue<int*byte[]>()

        let e = EventHandler (fun o e-> let value = ref (0, [||])
                                        while queue.TryDequeue(value) do
                                            let x, pixels = !value
                                            ignore <|  wb.Lock()

                                            try wb.WritePixels (Int32Rect (x, 0, 1, iheight), pixels, 4, 0)
                                            finally wb.Unlock()
                                        )

        let timer = 
            DispatcherTimer(
                TimeSpan.FromMilliseconds(200.)      ,
                DispatcherPriority.ApplicationIdle  ,
                e                                   ,
                window.Dispatcher
                )

        timer.Start()

        let tracers = 
            seq {
                for x in 0..iwidth - 1 -> 
                    async {
                        let row = [| for i in 0..iheight - 1 -> Black|]
                        for y in 0..iheight - 1 do
                            let vp = viewPort.ViewPoint (float x / width) (float y / height)
                            let ray = Ray.FromTo eye vp
                            row.[y] <- Trace ray world lights
    
                        let pixels = 
                            [| for i in row do
                                yield asByte i.Blue
                                yield asByte i.Green
                                yield asByte i.Red 
                                yield byte 255
                            |]

                        queue.Enqueue(x, pixels);

                        return ()
                    }
            }

        let tracer = 
            tracers
                |>  Async.Parallel 
                |>  Async.Ignore

        let sw = Stopwatch ()

        let complete = 
            async {
                do! tracer

                dispatch window.Dispatcher (fun () -> 
                    sw.Stop()
                    window.Title <- sprintf "Trace took %f secs" sw.Elapsed.TotalSeconds
                    )

                return ()
            }

        sw.Start()

        Async.Start complete

        let i = new Image ()
        i.Source <- wb
        window.Content <- i
        )

    let result = window.ShowDialog()

    0