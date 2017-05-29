module Language.C.Syntax.Libs
  ( -- math.h
    expE, expm1E, logE, log1pE, sqrtE,
    infinityE,negInfinityE,

    -- stdio.h
    printfE, sscanfE, fopenE, fcloseE, fileT, feofE, fgetsE, rewindE,

    -- stdlib.h
    randE, srandE, mallocE, freeE,

    -- Boehm Gargbage Collector
    gcHeader, gcInit, gcMalloc,

    -- OpenMP
    openMpHeader, ompGetNumThreads, ompGetThreadNum,

    -- MPI
    toMpiType, mpiHeader, mpiInit, mpiFinalize, mpiCommWorld, mpiCommSize,
    mpiCommRank, mpiSend, mpiReceive, MpiOp(..), mpiOpToCExpr, mpiReduce
  ) where

import Language.C.Syntax.AST

{-
  As a convention to make the CExpressions standout, functions that return CExpr
  have a suffix 'E' for instance 'printfE'
-}


--------------------------------------------------------------------------------
--                                 Lib C                                      --
--------------------------------------------------------------------------------
{-
  Here we have calls to a very small subset of functionality provided by libc.
  In the future, we should have a standard way to add in bindings to C
  libraries. Easily generating code for existing C libraries is one of the key
  design goals of pedantic-c
-}

------------
-- math.h --
------------

expE,expm1E,logE,log1pE,sqrtE :: CExpr -> CExpr
expE   = mkUnaryE "exp"
expm1E = mkUnaryE "expm1"
logE   = mkUnaryE "log"
log1pE = mkUnaryE "log1p"
sqrtE  = mkUnaryE "sqrt"

infinityE,negInfinityE :: CExpr
infinityE    = (intE 1) ./. (intE 0)
negInfinityE = logE (intE 0)

--------------
-- stdlib.h --
--------------

randE :: CExpr
randE = mkCallE "rand" []

srandE :: CExpr -> CExpr
srandE e = mkCallE "srand" [e]

mallocE :: CExpr -> CExpr
mallocE = mkUnaryE "malloc"

freeE :: CExpr -> CExpr
freeE = mkUnaryE "free"

--------------
-- stdio.h --
--------------

printfE,sscanfE :: [CExpr] -> CExpr
printfE = mkCallE "printf"
sscanfE = mkCallE "sscanf"

fopenE :: CExpr -> CExpr -> CExpr
fopenE e0 e1 = mkCallE "fopen" [e0,e1]

fcloseE,feofE,rewindE :: CExpr -> CExpr
fcloseE e = mkCallE "fclose" [e]
feofE e = mkCallE "feof" [e]
rewindE e = mkCallE "rewind" [e]

fgetsE :: CExpr -> CExpr -> CExpr -> CExpr
fgetsE e0 e1 e2 = mkCallE "fgets" [e0,e1,e2]

fileT :: CTypeSpec
fileT = CTypeDefType (Ident "FILE")

--------------------------------------------------------------------------------
--                            Boehm Garbage Collector                         --
--------------------------------------------------------------------------------
{-
   Currently needed for handling arrays and datum.

   In the future, an intermediate language based on the region calculus will be
   employed here.
-}

gcHeader :: Preprocessor
gcHeader = PPInclude "gc.h"

gcInit :: CExpr
gcInit = mkCallE "GC_INIT" []

gcMalloc :: CExpr -> CExpr
gcMalloc e = mkCallE "GC_MALLOC" [e]

--------------------------------------------------------------------------------
--                                  OpenMP                                    --
--------------------------------------------------------------------------------
{-
   For generating pragmas for shared memory parallelism, that is parallelism on
   on a single process that makes use of multithreaded processors. This
   interface is implemented in most C compilers and is accessed through pragmas
-}

openMpHeader :: Preprocessor
openMpHeader = PPInclude "omp.h"

ompGetNumThreads :: CExpr
ompGetNumThreads = mkCallE "omp_get_num_threads" []

ompGetThreadNum :: CExpr
ompGetThreadNum = mkCallE "omp_get_thread_num" []


--------------------------------------------------------------------------------
--                                    MPI                                     --
--------------------------------------------------------------------------------
{-
   Necessary bindings to the Message Passing Inferface for distributed programs
-}

-- just convered here are the ones necessary for Hakaru
toMpiType :: [CTypeSpec] -> CExpr
toMpiType (CInt:[])           = CVar . Ident $ "MPI_INT"
toMpiType (CDouble:[])        = CVar . Ident $ "MPI_DOUBLE"
toMpiType (CUnsigned:CInt:[]) = CVar . Ident $ "MPI_UNSIGNED"
toMpiType t = error $ "toMpiType{" ++ show t ++ "} is undefined"

mpiHeader :: Preprocessor
mpiHeader = PPInclude "mpi.h"

mpiInit :: CExpr -> CExpr -> CExpr
mpiInit argc argv = mkCallE "MPI_Init" [argc,argv]

mpiFinalize :: CExpr
mpiFinalize = mkCallE "MPI_Finalize" []

mpiCommWorld :: CExpr
mpiCommWorld = CVar . Ident $ "MPI_COMM_WORLD"

mpiCommSize :: CExpr -> CExpr
mpiCommSize e =
  mkCallE "MPI_Comm_size" [mpiCommWorld,address e]

mpiCommRank :: CExpr -> CExpr
mpiCommRank e =
  mkCallE "MPI_Comm_rank" [mpiCommWorld,address e]

mpiSend :: CExpr -> CExpr -> [CTypeSpec] -> CExpr -> CExpr -> CExpr
mpiSend buf count typ dest tag =
  mkCallE "MPI_Send" [buf,count,toMpiType typ,dest,tag,mpiCommWorld]

mpiReceive :: CExpr -> CExpr -> [CTypeSpec] -> CExpr -> CExpr -> CExpr
mpiReceive buf count typ source tag =
  mkCallE "MPI_Recv" [buf,count,toMpiType typ,source,tag
                     ,mpiCommWorld,CVar . Ident $ "MPI_STATUS_IGNORE"]

data MpiOp = MpiSum | MpiMul
  deriving (Show, Eq)

mpiOpToCExpr :: MpiOp -> CExpr
mpiOpToCExpr MpiSum = CVar . Ident $ "MPI_SUM"
mpiOpToCExpr MpiMul = CVar . Ident $ "MPI_PROD"

mpiReduce :: CExpr -> CExpr -> CExpr -> [CTypeSpec] -> MpiOp -> CExpr -> CExpr
mpiReduce sendbuf recvbuf count typ op root =
  mkCallE "MPI_Reduce" [sendbuf,recvbuf,count,toMpiType typ
                       ,mpiOpToCExpr op,root,mpiCommWorld]
