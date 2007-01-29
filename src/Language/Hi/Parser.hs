{-# OPTIONS -cpp -fglasgow-exts #-}
{-# OPTIONS -fno-warn-unused-matches -fno-warn-unused-binds -fno-warn-name-shadowing #-}
--
-- Copyright (C) 2004 Don Stewart - http://www.cse.unsw.edu.au/~dons
-- 
-- This library is free software; you can redistribute it and/or
-- modify it under the terms of the GNU Lesser General Public
-- License as published by the Free Software Foundation; either
-- version 2.1 of the License, or (at your option) any later version.
-- 
-- This library is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
-- Lesser General Public License for more details.
-- 
-- You should have received a copy of the GNU Lesser General Public
-- License along with this library; if not, write to the Free Software
-- Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307
-- USA
--

--
-- Based on $fptools/ghc/compiler/iface/BinIface.hs
--
--  (c) The University of Glasgow 2002
-- 
-- Binary interface file support.
--

--
-- This provides the "Binary" instances for the Iface type such that we
-- can parse binary representations of that type. i.e. from .hi files
--
-- The main problem we have is that all the stuff we don't care about,
-- we just want to read in to a string. So this has to be hand-hacked
-- somewhat.
--
-- The "Binary" class for hs-plugins only includes a get method. We
-- don't do any writing. Saves us having to properly reconstruct the
-- abstract syntax, which would pull in *way* too much of GHC.
--



module Language.Hi.Parser ( readIface, module Language.Hi.Syntax ) where

import Language.Hi.Syntax
import Language.Hi.Binary
import Language.Hi.FastString

#include "../../../config.h"

-- ---------------------------------------------------------------------------
-- how to get there from here

readIface :: FilePath -> IO Iface
readIface hi_path = getBinFileWithDict hi_path

-- ---------------------------------------------------------------------
-- All the Binary instances
--
-- Reading a binary interface into ParsedIface
--
-- We pull the trick of only reading up to the point we need
--

instance Binary Iface where
    get bh = do
            version   <- get bh :: IO String
            build_tag <- get bh :: IO String     -- 'way' flag

#if __GLASGOW_HASKELL__ >= 604
            pkg_name  <- get bh :: IO FastString
            mod_name  <- get bh :: IO FastString
            _is_boot  <- get bh :: IO Bool
#elif CABAL == 1 && __GLASGOW_HASKELL__ == 603
            mod_name <- get bh :: IO FastString
            let pkg_name = mkFastString "unknown"
#else /* <= 622 */
            mod_name  <- get bh :: IO FastString
            pkg_name  <- get bh :: IO FastString
#endif
            mod_vers  <- get bh :: IO Version
            orphan    <- get bh :: IO Bool
            deps      <- get bh :: IO Dependencies

            get bh :: IO (Bin Int) -- fake a lazyGet for [Usage]
            usages    <- get bh :: IO [Usage]

            exports   <- get bh :: IO [IfaceExport]

--      (exp_vers  :: Version)              <- get bh
--      (fixities  :: [(OccName,Fixity)])   <- get bh
--      (deprecs   :: [IfaceDeprec])        <- get bh

--      (decls     :: [(Version,IfaceDecl)])<- get bh

--      (insts     :: [IfaceInst])          <- get bh
--      (rules     :: [IfaceRule])          <- get bh
--      (rule_vers :: Version)              <- get bh

            return $ Iface {
                 mi_package   = unpackFS pkg_name,
                 mi_module    = unpackFS mod_name,
                 mi_deps      = deps ,
                 mi_usages    = usages,
                 mi_exports   = exports {-,-} 

--              mi_mod_vers  = mod_vers,
--              mi_boot      = False,  -- .hi files are never .hi-boot files!
--              mi_orphan    = orphan,
--              mi_usages    = usages,
--              mi_exports   = exports,
--              mi_exp_vers  = exp_vers,
--              mi_fixities  = fixities,
--              mi_deprecs   = deprecs,
--              mi_decls     = decls,
--              mi_insts     = insts,
--              mi_rules     = rules,
--              mi_rule_vers = rule_vers 
            }

------------------------------------------------------------------------
--
--        Types from: Iface.hs, HscTypes
--

-- fake a lazyGet
instance Binary Dependencies where
    get bh = do get bh :: IO (Bin Int)  -- really a BinPtr Int
                ms <- get bh :: IO [(FastString,Bool)]
                ps <- get bh :: IO [FastString]
                _  <- get bh :: IO [FastString]         -- !!orphans
                return Deps { dep_mods  = map unpackFS $! map fst ms, 
                              dep_pkgs  = map unpackFS ps {-,-}
                       }

------------------------------------------------------------------------
-- Usages
------------------------------------------------------------------------

instance Binary OccName where
        get bh = do aa <- get bh :: IO NameSpace
                    ab <- get bh :: IO FastString
                    return $ OccName aa (unpackFS ab)

instance Binary NameSpace where
        get bh = do h <- getByte bh
                    case h of
                      0 -> return VarName
                      1 -> return DataName
                      2 -> return TvName
                      _ -> return TcClsName

instance Binary Usage where
    get bh = do (nm   :: FastString)                <- get bh
                (mod  :: Version)                   <- get bh
                (exps :: Maybe Version)             <- get bh
                (ents :: [(OccName,Version)])       <- get bh
                (rules :: Version)                  <- get bh
                return $ Usage {usg_name = (unpackFS nm), 
                                usg_mod = mod,
                                usg_exports = exps, 
                                usg_entities = ents,
                                usg_rules = rules }

------------------------------------------------------------------------
-- Exports

instance (Binary name) => Binary (GenAvailInfo name) where
    get bh = do
        h <- getByte bh
        case h of
          0 -> do (aa :: name) <- get bh
                  return $ Avail aa
          _ -> do (ab :: name)   <- get bh
                  (ac :: [name]) <- get bh
                  return $ AvailTC ab ac

{-
instance Binary a => Binary (Deprecs a) where
    get bh = do
        h <- getByte bh
        case h of
          0 -> return Deprecs
          1 -> do (aa :: FastString) <- get bh
                  return Deprecs
          _ -> do (ab :: a) <- get bh
                  return Deprecs
-}

-------------------------------------------------------------------------
--        Types from: BasicTypes
-------------------------------------------------------------------------

{-
instance Binary Activation where
    get bh = do
        h <- getByte bh
        case h of
          0 -> return Activation
          1 -> return Activation
          2 -> do (aa :: Int) <- get bh ; return Activation
          _ -> do (ab :: Int) <- get bh ; return Activation

instance Binary StrictnessMark where
    get bh = do
        h <- getByte bh
        case h of
          0 -> return StrictnessMark
          1 -> return StrictnessMark
          _ -> return StrictnessMark

instance Binary Boxity where
    get bh = do
        h <- getByte bh
        case h of
          0 -> return Boxity
          _ -> return Boxity

instance Binary TupCon where
    get bh = do
      (ab :: Boxity) <- get bh
      (ac :: Arity)  <- get bh
      return TupCon

instance Binary RecFlag where
    get bh = do
        h <- getByte bh
        case h of
          0 -> return RecFlag
          _ -> return RecFlag

instance Binary DefMeth where
    get bh = do
        h <- getByte bh
        case h of
          0 -> return DefMeth
          1 -> return DefMeth
          _ -> return DefMeth

instance Binary FixityDirection where
    get bh = do
        h <- getByte bh
        case h of
          0 -> return FixityDirection
          1 -> return FixityDirection
          _ -> return FixityDirection

instance Binary Fixity where
    get bh = do
      (aa :: Int)             <- get bh
      (ab :: FixityDirection) <- get bh
      return Fixity

instance (Binary name) => Binary (IPName name) where
    get bh = do
        h <- getByte bh
        case h of
          0 -> do (aa :: name) <- get bh ; return IPName
          _ -> do (ab :: name) <- get bh ; return IPName

-------------------------------------------------------------------------
--        Types from: basicTypes/NewDemand
-------------------------------------------------------------------------

instance Binary DmdType where
    -- Ignore DmdEnv when spitting out the DmdType
    get bh = do (ds :: [Demand])  <- get bh
                (dr :: DmdResult) <- get bh
                return DmdType

instance Binary Demand where
    get bh = do
        h <- getByte bh
        case h of
          0 -> return Demand
          1 -> return Demand
          2 -> do (aa :: Demand)  <- get bh ; return Demand
          3 -> do (ab :: Demands) <- get bh ; return Demand
          4 -> do (ac :: Demands) <- get bh ; return Demand
          5 -> do (ad :: Demand)  <- get bh ; return Demand
          _ -> return Demand

instance Binary Demands where
    get bh = do
        h <- getByte bh
        case h of
          0 -> do (aa :: Demand)   <- get bh
                  return Demands
          _ -> do (ab :: [Demand]) <- get bh
                  return Demands

instance Binary DmdResult where
    get bh = do
        h <- getByte bh
        case h of
          0 -> return DmdResult
          1 -> return DmdResult
          _ -> return DmdResult

instance Binary StrictSig where
        get bh = do (aa :: DmdType) <- get bh ; return StrictSig
-}

-------------------------------------------------------------------------
--        Types from: CostCentre, from profiling/CostCentre.lhs
-------------------------------------------------------------------------

{-
instance Binary IsCafCC where
    get bh = do
        h <- getByte bh
        case h of
          0 -> return IsCafCC
          _ -> return IsCafCC

instance Binary IsDupdCC where
    get bh = do
        h <- getByte bh
        case h of
          0 -> return IsDupdCC
          _ -> return IsDupdCC

instance Binary CostCentre where
    get bh = do
        h <- getByte bh
        case h of
          0 -> do return CostCentre
          1 -> do (aa :: CcName)        <- get bh
                  (ab :: ModuleName)    <- get bh
                  (ac :: IsDupdCC)      <- get bh
                  (ad :: IsCafCC)       <- get bh
                  return CostCentre
          _ -> do (ae :: ModuleName)    <- get bh
                  return CostCentre
-}

-------------------------------------------------------------------------
--        IfaceTypes and friends, from IfaceType.lhs
-------------------------------------------------------------------------

{-
instance Binary IfaceExtName where
    get bh = do
        h <- getByte bh
        case h of
          0 -> do (mod :: ModuleName) <- get bh
                  (occ :: OccName)    <- get bh
                  return IfaceExtName
          1 -> do (mod :: ModuleName) <- get bh
                  (occ :: OccName)    <- get bh
                  (vers :: Version)   <- get bh
                  return IfaceExtName
          _ -> do (occ :: OccName)    <- get bh
                  return IfaceExtName

instance Binary IfaceBndr where
    get bh = do
        h <- getByte bh
        case h of
          0 -> do (aa :: IfaceIdBndr) <- get bh ; return IfaceBndr
          _ -> do (ab :: IfaceTvBndr) <- get bh ; return IfaceBndr

instance Binary Kind where
    get bh = do
        h <- getByte bh
        case h of
          0 -> return Kind
          1 -> return Kind
          2 -> return Kind
          3 -> return Kind
          4 -> return Kind
          _ -> do (k1 :: Kind) <- get bh
                  (k2 :: Kind) <- get bh
                  return Kind

instance Binary IfaceType where
    get bh = do
        h <- getByte bh
        case h of
          0 -> do (aa :: IfaceTvBndr)   <- get bh
                  (ab :: IfaceType)     <- get bh
                  return IfaceType
          1 -> do (ad :: OccName)       <- get bh
                  return IfaceType
          2 -> do (ae :: IfaceType)     <- get bh
                  (af :: IfaceType)     <- get bh
                  return IfaceType
          3 -> do (ag :: IfaceType)     <- get bh
                  (ah :: IfaceType)     <- get bh
                  return IfaceType
          5 -> do (ap :: IfacePredType) <- get bh
                  return IfaceType

        -- Now the special cases for TyConApp
          6 -> return IfaceType
          7 -> return IfaceType
          8 -> return IfaceType
          9 -> do (ty :: IfaceType)     <- get bh
                  return IfaceType
          10 -> return IfaceType
          11 -> do (t1 :: IfaceType)    <- get bh
                   (t2 :: IfaceType)    <- get bh
                   return IfaceType
          12 -> do (tc :: IfaceExtName) <- get bh
                   (tys :: [IfaceType]) <- get bh
                   return IfaceType
          _  -> do (tc :: IfaceTyCon)   <- get bh
                   (tys :: [IfaceType]) <- get bh
                   return IfaceType

instance Binary IfaceTyCon where
   get bh = do
    h <- getByte bh
    case h of
      1 -> return IfaceTyCon
      2 -> return IfaceTyCon
      _ -> do (bx :: Boxity) <- get bh
              (ar :: Arity)  <- get bh
              return IfaceTyCon

instance Binary IfacePredType where
    get bh = do
        h <- getByte bh
        case h of
          0 -> do (aa :: IfaceExtName) <- get bh
                  (ab :: [IfaceType])  <- get bh
                  return IfacePredType
          _ -> do (ac :: (IPName OccName))      <- get bh
                  (ad :: IfaceType)             <- get bh
                  return IfacePredType

instance Binary IfaceExpr where
    get bh = do
        h <- getByte bh
        case h of
          0 -> do (aa :: OccName)       <- get bh
                  return IfaceExpr
          1 -> do (ab :: IfaceType)     <- get bh
                  return IfaceExpr
          2 -> do (ac :: Boxity)        <- get bh
                  (ad :: [IfaceExpr])   <- get bh
                  return IfaceExpr
          3 -> do (ae :: IfaceBndr)     <- get bh
                  (af :: IfaceExpr)     <- get bh
                  return IfaceExpr
          4 -> do (ag :: IfaceExpr)     <- get bh
                  (ah :: IfaceExpr)     <- get bh
                  return IfaceExpr
          5 -> do (ai :: IfaceExpr)     <- get bh
                  (aj :: OccName)       <- get bh
                  (ak :: [IfaceAlt])    <- get bh
                  return IfaceExpr
          6 -> do (al :: IfaceBinding)  <- get bh
                  (am :: IfaceExpr)     <- get bh
                  return IfaceExpr
          7 -> do (an :: IfaceNote)     <- get bh
                  (ao :: IfaceExpr)     <- get bh
                  return IfaceExpr
          8 -> do (ap :: Literal)       <- get bh
                  return IfaceExpr
          9 -> do (as :: ForeignCall)   <- get bh
                  (at :: IfaceType)     <- get bh
                  return IfaceExpr
          _ -> do (aa :: IfaceExtName)  <- get bh
                  return IfaceExpr

instance Binary IfaceConAlt where
    get bh = do
        h <- getByte bh
        case h of
          0 -> return IfaceConAlt
          1 -> do (aa :: OccName) <- get bh
                  return IfaceConAlt
          2 -> do (ab :: Boxity)  <- get bh
                  return IfaceConAlt
          _ -> do (ac :: Literal) <- get bh
                  return IfaceConAlt

instance Binary IfaceBinding where
    get bh = do
        h <- getByte bh
        case h of
          0 -> do (aa :: IfaceIdBndr) <- get bh
                  (ab :: IfaceExpr)   <- get bh
                  return IfaceBinding
          _ -> do (ac :: [(IfaceIdBndr,IfaceExpr)]) <- get bh
                  return IfaceBinding

instance Binary IfaceIdInfo where
    get bh = do
        h <- getByte bh
        case h of
          0 -> return IfaceIdInfo
          _ -> do (info :: [IfaceInfoItem]) <- lazyGet bh
                  return IfaceIdInfo

instance Binary IfaceInfoItem where
    get bh = do
        h <- getByte bh
        case h of
          0 -> do (aa :: Arity)         <- get bh
                  return IfaceInfoItem
          1 -> do (ab :: StrictSig)     <- get bh
                  return IfaceInfoItem
          2 -> do (ac :: Activation)    <- get bh
                  (ad :: IfaceExpr)     <- get bh
                  return IfaceInfoItem
          3 -> return IfaceInfoItem
          _ -> do (ae :: IfaceExtName) <- get bh
                  (af :: Arity)        <- get bh
                  return IfaceInfoItem

instance Binary IfaceNote where
    get bh = do
        h <- getByte bh
        case h of
          0 -> do (aa :: CostCentre) <- get bh
                  return IfaceNote
          1 -> do (ab :: IfaceType ) <- get bh
                  return IfaceNote
          2 -> return IfaceNote
          3 -> return IfaceNote
          _ -> do (ac :: String) <- get bh
                  return IfaceNote

instance Binary IfaceDecl where
    get bh = do
        h <- getByte bh
        case h of
          0 -> do 
            (name :: OccName)           <- get bh
            (ty :: IfaceType)           <- get bh
            (idinfo :: IfaceIdInfo)     <- get bh
            return IfaceDecl
          1 -> error "Binary.get(TyClDecl): ForeignType"
          2 -> do
            (a1 :: IfaceContext)  <- get bh
            (a2 :: OccName)       <- get bh
            (a3 :: [IfaceTvBndr]) <- get bh
            (a4 :: IfaceConDecls) <- get bh
            (a5 :: RecFlag)       <- get bh
            (a6 :: ArgVrcs)       <- get bh
            (a7 :: Bool)          <- get bh
            return IfaceDecl
          3 -> do
            (aq :: OccName)       <- get bh
            (ar :: [IfaceTvBndr]) <- get bh
            (as :: ArgVrcs)       <- get bh
            (at :: IfaceType)     <- get bh
            return IfaceDecl
          _ -> do
            (a1 :: IfaceContext)    <- get bh
            (a2 :: OccName)         <- get bh
            (a3 :: [IfaceTvBndr])   <- get bh
            (a4 :: [FunDep OccName])<- get bh
            (a5 :: [IfaceClassOp])  <- get bh
            (a6 :: RecFlag)         <- get bh
            (a7 :: ArgVrcs)         <- get bh
            return IfaceDecl

instance Binary IfaceInst where
    get bh = do 
        (ty :: IfaceType) <- get bh
        (dfun :: OccName) <- get bh
        return IfaceInst

instance Binary IfaceConDecls where
    get bh = do
        h <- getByte bh
        case h of
          0 -> return IfaceConDecls
          1 -> do (aa :: [IfaceConDecl]) <- get bh
                  return IfaceConDecls
          _ -> do (aa :: IfaceConDecl)   <- get bh
                  return IfaceConDecls

instance Binary IfaceConDecl where
    get bh = do
        (a1 :: OccName)         <- get bh
        (a2 :: [IfaceTvBndr])   <- get bh
        (a3 :: IfaceContext)    <- get bh
        (a4 :: [IfaceType])     <- get bh
        (a5 :: [StrictnessMark])<- get bh
        (a6 :: [OccName])       <- get bh
        return IfaceConDecl

instance Binary IfaceClassOp where
   get bh = do
        (n   :: OccName)   <- get bh
        (def :: DefMeth)   <- get bh
        (ty  :: IfaceType) <- get bh
        return IfaceClassOp

instance Binary IfaceRule where
    get bh = do
        (a1 :: RuleName)        <- get bh
        (a2 :: Activation)      <- get bh
        (a3 :: [IfaceBndr])     <- get bh
        (a4 :: IfaceExtName)    <- get bh
        (a5 :: [IfaceExpr])     <- get bh
        (a6 :: IfaceExpr)       <- get bh
        return IfaceRule

-}

------------------------------------------------------------------------
-- from Literal
------------------------------------------------------------------------

{-
instance Binary Literal where
    get bh = do
	    h <- getByte bh
	    case h of
	      0 -> do
		    (aa :: Char) <- get bh
		    return Literal
	      1 -> do
		    (ab :: FastString) <- get bh
		    return Literal
	      2 -> do return Literal
	      3 -> do
		    (ad :: Integer) <- get bh
		    return Literal
	      4 -> do
		    (ae :: Integer) <- get bh
		    return Literal
	      5 -> do
		    (af :: Integer) <- get bh
		    return Literal
	      6 -> do
		    (ag :: Integer) <- get bh
		    return Literal
	      7 -> do
		    (ah :: Rational) <- get bh
		    return Literal
	      8 -> do
		    (ai :: Rational) <- get bh
		    return Literal
	      9 -> do
		    (aj :: FastString) <- get bh
		    (mb :: Maybe Int)  <- get bh
		    return Literal
              _ -> return Literal -- ?

-}

------------------------------------------------------------------------
-- prelude/ForeignCall.lhs
------------------------------------------------------------------------

{-
instance Binary ForeignCall where
    get bh = do
	    h <- getByte bh
	    case h of
	      0 -> do (aa :: CCallSpec) <- get bh
		      return ForeignCall
	      _ -> do (ab :: DNCallSpec) <- get bh
		      return ForeignCall

instance Binary Safety where
    get bh = do
	    h <- getByte bh
	    case h of
	      0 -> do (aa :: Bool) <- get bh
		      return Safety
	      _ -> return Safety

instance Binary CExportSpec where
    get bh = do
	  (aa :: CLabelString) <- get bh
	  (ab :: CCallConv)    <- get bh
	  return CExportSpec

instance Binary CCallSpec where
    get bh = do
	  (aa :: CCallTarget) <- get bh
	  (ab :: CCallConv)   <- get bh
	  (ac :: Safety)      <- get bh
	  return CCallSpec

instance Binary CCallTarget where
    get bh = do
	    h <- getByte bh
	    case h of
	      0 -> do (aa :: CLabelString) <- get bh
		      return CCallTarget
	      _ -> return CCallTarget

instance Binary CCallConv where
    get bh = do
	    h <- getByte bh
	    case h of
	      0 -> return CCallConv
	      _ -> return CCallConv

instance Binary DNCallSpec where
    get bh = do
          (isStatic :: Bool)  <- get bh
	  (kind   :: DNKind)  <- get bh
	  (ass   :: String)   <- get bh
	  (nm    :: String)   <- get bh
	  return DNCallSpec

instance Binary DNKind where
    get bh = do
	    h <- getByte bh
	    case h of
	      _ -> return DNKind

instance Binary DNType where
    get bh = do
	    h <- getByte bh
	    case h of
	      _ -> return DNType

-}
