#lang typed/synthcl
(require typed/lib/roseunit)

; We are using the sobelFilter5 function from reference.rkt as a reference
; sequential implementation for the Sobel filter.  This reference implementation 
; was derived via a multi-step refinment process from the implementation provided 
; by Samsung, using a combination of verification and synthesis.
;
; We represent images as arrays of ints, since chars are not supported. The reference 
; implementation applies the Sobel filter to a width x height x pixelSize image,
; represented as a flat array of ints.  The implementation works for pixelSize = 4 * (sizeof int).
(procedure int* (sobelFilter [int* inputImage] [int width] [int height] [int pixelSize])
                 
  (: int* outputImage)
  (= outputImage ((int*) (malloc (* width height pixelSize))))
  (memset outputImage 0 (* width height pixelSize))
  
  (: int gx gy w i i00 i01 i02 i10 i11 i12 i20 i21 i22)
  (= w (* width 4))
  
  (for [(: int y in (range 1 (- height 1)))  ; [Y]
        (: int x in (range 4 (- w 4)))]      ; [X]
    (= i (+ (* y w) x))
    (= i00 [inputImage (- i 4 w)])
    (= i01 [inputImage (- i w)])
    (= i02 [inputImage (- (+ i 4) w)])
    (= i10 [inputImage (- i 4)])
    (= i11 [inputImage i])
    (= i12 [inputImage (+ i 4)])
    (= i20 [inputImage (+ (- i 4) w)])
    (= i21 [inputImage (+ i w)])
    (= i22 [inputImage (+ i 4 w)])
    (= gx (+ i00 (* 2 i01) i02 (* -1 i20) (* -2 i21) (* -1 i22)))
    (= gy (+ i00 (* -1 i02) (* 2 i10) (* -2 i12) i20 (* -1 i22)))
    (= [outputImage i] ((int) (/ (sqrt (+ (* ((float) gx) gx) (* ((float) gy) gy))) 2))))
  
  outputImage)

; A host implementation for a scalar Sobel filter.  This is a straightforward adaption of the 
; reference implementation, in which the loop body is placed into a kernel and the loop bounds 
; are expressed with a suitable global offset and work size.  We assume that width and height are 
; both at least 3.
(procedure int* (sobelFilterScalarHost [int* inputImage] [int width] [int height] [int pixelSize])
  (: cl_context context)
  (: cl_command_queue command_queue)
  (: cl_program program)
  (: cl_kernel kernel)
  (: cl_mem inputImageBuffer outputImageBuffer)
  (: int* outputImage)
  (: int size w)
  (: int[2] global offset)
  
  (= w (* width 4))
  (= [global 0] (- w 8))      ; see line [X] of sobelFilter
  (= [offset 0] 4)
  (= [global 1] (- height 2)) ; see line [Y] of sobelFilter
  (= [offset 1] 1)
  
  (= size (* width height pixelSize))
  (= outputImage ((int*) (malloc size)))
  (memset outputImage 0 size)
  
  (= context (clCreateContext))
  
  (= command_queue (clCreateCommandQueue context))
 
  (= inputImageBuffer (clCreateBuffer context CL_MEM_READ_ONLY size))
  (= outputImageBuffer (clCreateBuffer context CL_MEM_WRITE_ONLY size))
  
  (= program (clCreateProgramWithSource context "sobel-kernel.rkt"))
  
  (clEnqueueWriteBuffer command_queue inputImageBuffer 0 size inputImage)
  (clEnqueueWriteBuffer command_queue outputImageBuffer 0 size outputImage)
  
  (= kernel (clCreateKernel program "sobelFilterScalarKernel"))
  (clSetKernelArg kernel 0 inputImageBuffer)
  (clSetKernelArg kernel 1 outputImageBuffer)
  (clSetKernelArg kernel 2 w)

  (clEnqueueNDRangeKernel command_queue kernel 2 offset global NULL)
  (clEnqueueReadBuffer command_queue outputImageBuffer 0 size outputImage)
  outputImage)

; A host implementation for a vectorized Sobel filter.  This is a straightforward adaption of the 
; scalar parallelized implementation, in which we change the type of the array from int to int4, 
; and we adjust the iteration space accordingly (by dividing the x dimensions by 4).  We assume 
; that width and height are both at least 3.
(procedure int* (sobelFilterVectorHost [char* name] [int* inputImage] [int width] [int height] [int pixelSize])
  (: cl_context context)
  (: cl_command_queue command_queue)
  (: cl_program program)
  (: cl_kernel kernel)
  (: cl_mem inputImageBuffer outputImageBuffer)
  (: int* outputImage)
  (: int size)
  (: int[2] global offset)
  
  (= [global 0] (- width 2))  ; see line [X] of sobelFilter:  we divide x size and offset by 4 due to the use of int4 datatype
  (= [offset 0] 1)
  (= [global 1] (- height 2)) ; see line [Y] of sobelFilter
  (= [offset 1] 1)
  
  (= size (* width height pixelSize))
  (= outputImage ((int*) (malloc size)))
  (memset outputImage 0 size)
  
  (= context (clCreateContext))
  
  (= command_queue (clCreateCommandQueue context))
 
  (= inputImageBuffer (clCreateBuffer context CL_MEM_READ_ONLY size))
  (= outputImageBuffer (clCreateBuffer context CL_MEM_WRITE_ONLY size))
  
  (= program (clCreateProgramWithSource context "sobel-kernel.rkt"))
  
  (clEnqueueWriteBuffer command_queue inputImageBuffer 0 size inputImage)
  (clEnqueueWriteBuffer command_queue outputImageBuffer 0 size outputImage)
  
  (= kernel (clCreateKernel program name))
  (clSetKernelArg kernel 0 inputImageBuffer)
  (clSetKernelArg kernel 1 outputImageBuffer)
  (clSetKernelArg kernel 2 width)

  (clEnqueueNDRangeKernel command_queue kernel 2 offset global NULL)
  (clEnqueueReadBuffer command_queue outputImageBuffer 0 size outputImage)
  outputImage)

; Given two arrays of the same size, checks that they hold the same 
; values at each index.
(procedure void (check [int* actual] [int* expected] [int SIZE])
  (assert (>= SIZE 0))
  (for [(: int i in (range SIZE))]
    (assert (== [actual i] [expected i]))))

(: int pixelSize)
(= pixelSize (* 4 (sizeof int)))

; Verifies that sobelFilterScalarHost and sobelFilter are equivalent on all 
; images with dimensions ranging from 3..10 x 3..10 (~26 sec).
(procedure void (verify_scalar)
  (verify #:forall [(: int width in (range 3 10))
                    (: int height in (range 3 10))
                    (: int[(* width height pixelSize)] inputImage)]
          #:ensure (check (sobelFilterScalarHost inputImage width height pixelSize)
                          (sobelFilter inputImage width height pixelSize)
                          (* width height pixelSize))))

; Verifies that sobelFilterVectorHost and sobelFilter are equivalent on all 
; images with dimensions ranging from 3..10 x 3..10 (~26 sec).
(procedure void (verify_vectorized)
  (verify #:forall [(: int width in (range 3 10))
                    (: int height in (range 3 10))
                    (: int[(* width height pixelSize)] inputImage)]
          #:ensure (check (sobelFilterVectorHost "sobelFilterVectorKernel" inputImage width height pixelSize)
                          (sobelFilter inputImage width height pixelSize)
                          (* width height pixelSize))))

; Synthesizes the missing constants in sobelFilterVectorKernel so that 
; sobelFilterVectorHost and sobelFilter are equivalent on all 
; images with dimensions 4x4 (~16 sec).  The solution is correct for all 
; sizes, which can be verified using the verify form.
(procedure void (synth_vectorized)
  (synth #:forall [(: int width in (range 4 5))
                   (: int height in (range 4 5))
                   (: int[(* width height pixelSize)] inputImage)]
         #:bitwidth 9
         #:ensure (check (sobelFilterVectorHost "sobelFilterVectorKernelSketch" inputImage width height pixelSize)
                         (sobelFilter inputImage width height pixelSize)
                         (* width height pixelSize))))

(check-type+output (verify_scalar) -> "no counterexample found")
(check-type+output (verify_vectorized) -> "no counterexample found")
(check-type+output
 (synth_vectorized)
 -> "(kernel\n  void\n  (sobelFilterVectorKernelSketch\n   (int4* inputImage)\n   (int4* outputImage)\n   (int w))\n  (: int x y i offset)\n  (: int4 i00 i01 i02 i10 i11 i12 i20 i21 i22)\n  (: float4 gx gy)\n  (= x (get_global_id 0))\n  (= y (get_global_id 1))\n  (= i (+ (* y w) x))\n  (= offset 1)\n  (= i00 (inputImage (- i offset w)))\n  (= i01 (inputImage (- i w)))\n  (= i02 (inputImage (- (+ i offset) w)))\n  (= i10 (inputImage (- i offset)))\n  (= i11 (inputImage i))\n  (= i12 (inputImage (+ i offset)))\n  (= i20 (inputImage (+ (- i offset) w)))\n  (= i21 (inputImage (+ i w)))\n  (= i22 (inputImage (+ i offset w)))\n  (=\n   gx\n   (convert_float4 (+ i00 (* 2 i01) i02 (* -1 i20) (* -2 i21) (* -1 i22))))\n  (=\n   gy\n   (convert_float4 (+ i00 (* -1 i02) (* 2 i10) (* -2 i12) i20 (* -1 i22))))\n  (=\n   (outputImage i)\n   (convert_int4 (/ (sqrt (+ (* gx gx) (* gy gy))) ((float4) 2)))))")

#|
(: int[(* 9 4)] input)
(for [(: int i in (range (* 9 4)))]
    (= [input i] i))
(sobelFilter input 3 3 4)
(sobelFilterHost input 3 3 4)
|#

#|
(procedure int* (runRef [int width] [int height])
  (: int* input)
  (= input ((int*) (malloc (* width height 4))))
  (for [(: int i in (range (* width height 4)))]
    (= [input i] i))
  (sobelFilter input width height 4))|#
           
