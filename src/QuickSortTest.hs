{-# LANGUAGE ForeignFunctionInterface #-}

-- ffi beg 
import Foreign
import Foreign.Storable
import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String
import System.IO.Unsafe
import Control.Monad
import Control.Lens
import AronModule

import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as VM
-- ffi end

-- KEY: haskell ffi, foreign function interface
foreign import ccall "increment" 
    c_increment :: CInt -> IO CInt

f_increment :: Int  -> IO Int
f_increment n = do 
                 x <- c_increment (fromIntegral n)
                 return  $ fromIntegral x

foreign import ccall "print_ascii" 
    c_print_ascii :: IO() 

f_print_ascii::IO()
f_print_ascii = c_print_ascii

foreign import ccall "alloca_memory"
    c_alloca_memory::Ptr CInt -> CInt -> IO()

foreign import ccall "dotn"
    c_dotn:: CInt -> Ptr CDouble -> Ptr CDouble -> IO CDouble

f_dot :: [CDouble] -> [CDouble] -> IO CDouble
f_dot s1 s2 = do
  if length s1 /= length s2 then error "ERROR441: the length of two lists must be the same." else do
    let n = (fromIntegral . length) s1
    withArrayLen s1 $ \l1 pt1 -> 
      withArrayLen s2 $ \l2 pt2 -> 
        c_dotn n pt1 pt2 
    
foreign import ccall "addVec"
    c_addVec:: CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO()

foreign import ccall "subVec3"
    c_subVec3:: Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO()

foreign import ccall "mulScaleVec3"
    c_mulScaleVec3:: CDouble -> Ptr CDouble -> Ptr CDouble -> IO()

foreign import ccall "mulVec3Scale"
    c_mulVec3Scale:: Ptr CDouble -> CDouble -> Ptr CDouble -> IO()

foreign import ccall "multiVec"
    c_multiVec:: CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO()

foreign import ccall "multiMat"
    c_multiMat:: CInt -> CInt -> Ptr CDouble -> CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> IO()



foreign import ccall "quickSortInt"
    c_quickSortInt:: CInt -> CInt -> Ptr CInt -> IO()

f_quickSortInt :: [Int] -> IO [Int]
f_quickSortInt cx = do
  let cx' = map fromIntegral cx
  let vs = V.fromList (cx')
  v <- V.thaw vs
  VM.unsafeWith v $ \pt -> 
    c_quickSortInt 0 (len cx' - 1) pt
  out <- V.freeze v <&> V.toList <&> map fromIntegral
  return out

  

f_multiMat :: [[Double]] -> [[Double]] -> IO [[Double]]
f_multiMat cs cx = do
  let fx = fromIntegral
  let (h, w) = dim cs
  let (h', w') = dim cx 
  if w == w' && h == h' then do
    let cs' = map realToFrac $ join cs
    let cx' = map realToFrac $ join cx
    let nBytes = let n = 0 :: CDouble in w * h * sizeOf n
    allocaBytes nBytes $ \pt -> do
      withArrayLen cs' $ \l1 pt1 -> do
        withArrayLen cx' $ \l2 pt2 -> do
          c_multiMat (fx h) (fx w) pt1 (fx h') (fx w') pt2 pt 
          arr <- peekArray (w * h) pt
          return $ fxx w $ map realToFrac arr 
  else do
    error $ "ERROR114, Invalid dimension, w == w' && h == h'" 
  where
    dim x = (h, w) 
      where
        w = case x of
             [] -> 0
             vs:_ -> length vs
        h = length x 

fxx :: Int -> [a] -> [[a]]
fxx _ [] = []
fxx n cx = let (x, cx') = splitAt n cx in x : fxx n cx' 
 


-- int ret = gsl_poly_solve_quadratic_x(a, b, c, &x0, &x1); 
-- foreign import ccall "gsl_poly_solve_quadratic_x"
--     c_gsl_poly_solve_quadratic_x :: CDouble -> CDouble -> CDouble -> Ptr CDouble -> Ptr CDouble -> IO CInt


f_multiVec :: [[Double]] -> [Double] -> IO [Double]
f_multiVec cx v = do
  if length cx > 0 then do
    let nRow = length cx
    let nRow' = fromIntegral nRow
    let nCol = (length . head) cx
    let nCol' = fromIntegral nCol 
    when (nCol /= length v) $ do
      error "ERROR111: Invalid Input dimension, nCol /= length v"
    let cx' = map realToFrac $ join cx
    let v' = map realToFrac v
    let nBytes = nCol * sizeOf (0::CDouble)
    allocaBytes nBytes $ \u -> do
      withArrayLen cx' $ \l1 pt1 -> do
        withArrayLen v' $ \l2 pt2 -> do
          c_multiVec nRow' nCol' pt1 pt2 u
          arr <- peekArray nCol u
          return $ map realToFrac arr 
    else error "ERROR112: Invalid Input, f_multiVec"


-- f_gsl_poly_solve_quadratic :: Double -> Double -> Double -> IO(Double, Double, Int)
-- f_gsl_poly_solve_quadratic a b c = do 
--   let a' = realToFrac a
--   let b' = realToFrac b 
--   let c' = realToFrac c 
--   let x = 0.0 :: CDouble
--   let n0 = 1 * sizeOf x
--   let n1 = 1 * sizeOf x
--   allocaBytes n0 $ \x0 -> do 
--     allocaBytes n1 $ \x1 -> do 
--       ret <- c_gsl_poly_solve_quadratic_x a' b' c' x0 x1
--       x0' <- peekArray 1 x0
--       x1' <- peekArray 1 x1
--       return $ (realToFrac $ head x0', realToFrac $ head x1', fromIntegral ret)
   

f_addVec :: [Double] -> [Double] -> IO [Double]
f_addVec s1 s2 = do
  if length s1 /= length s2 then error "ERROR334: the length of two lists must be the same." 
   else do
    let n = length s1
    let s1' = map realToFrac s1
    let s2' = map realToFrac s2
    let nBytes = let a = 0.0 :: CDouble in n * sizeOf a 
    allocaBytes nBytes $ \ptx -> do
      withArrayLen s1' (\l1 pt1 -> withArrayLen s2' (\l2 pt2 -> c_addVec (fromIntegral l1) pt1 pt2 ptx))
      arr <- peekArray n ptx 
      return $ map realToFrac arr

f_subVec3 :: [CDouble] -> [CDouble] -> IO [CDouble]
f_subVec3 s1 s2 = do
  let n = 3
  let nBytes = let a = 1.0 :: CDouble in n * sizeOf a
  allocaBytes nBytes $ \ptx -> do
    withArrayLen s1 $ \l1 pt1 -> 
      withArrayLen s2 $ \l2 pt2 ->
        c_subVec3 pt1 pt2 ptx
    arr <- peekArray n ptx 
    return arr

f_mulScaleVec3 :: CDouble -> [CDouble] -> IO [CDouble]
f_mulScaleVec3 x s1 = do
  -- 3 elements list
  let n = 3
  -- total of bytes for 3 elements
  let nBytes = let a = 1.0 :: CDouble in n * sizeOf a
  allocaBytes nBytes $ \ptx -> do
    withArrayLen s1 $ \l1 pt1 -> 
      c_mulScaleVec3 x pt1 ptx
    arr <- peekArray n ptx
    return arr

f_mulVec3Scale :: [CDouble] -> CDouble -> IO [CDouble]
f_mulVec3Scale s1 x = f_mulScaleVec3 x s1 

{-|
-- NOTE: Need to free(pt) here
f_alloca_memory::Int -> IO [Int]
f_alloca_memory n = do
  pt <- mallocBytes n
  c_alloca_memory pt (fromIntegral n)
  arr <- peekArray n pt
  return $ map fromIntegral arr
-}

-- pt is freed after (Ptr a -> IO b) is terminated
f_alloca_memory::Int -> IO [Int]
f_alloca_memory n = do
  allocaBytes n $ \pt -> do
    c_alloca_memory pt (fromIntegral n)
    arr <- peekArray n pt
    return $ map fromIntegral arr

main = do 
        when True $ do
          fw "QuickSortTest.hs"          
          ls <- randomIntList (10^7) (1, 1000)
          old <- timeNowMicro
          lt <- f_quickSortInt ls
          new <- timeNowMicro
          -- print ls
          print $ len lt
          print $ show (last lt)
          print $ show (head lt)
          let diff = rf (new - old)/1000
          print diff          
