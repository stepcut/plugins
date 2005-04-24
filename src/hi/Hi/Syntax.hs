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
-- Based on code from $fptools/ghc/compiler/main/HscTypes.lhs
--  (c) The University of Glasgow 2002
--
 
module Hi.Syntax where

import Hi.FastString

import Data.List        ( intersperse )

-- ---------------------------------------------------------------------
-- An Iface, the representation of an .hi file.
--
-- The abstract syntax that we don't need is blanked with a default
-- type, however we must be careful in BinIface to still parse the
-- correct number of bytes for each data type. This involves leaving the
-- code alone, other than to add the types of the sub-constructors of
-- the types we have blanked out (because they can't be inferred
-- anymore).
--

data Iface = Iface {
        mi_package  :: String,          -- what package is this?
        mi_module   :: String,          -- what module is this?
        mi_deps     :: Dependencies,
        mi_usages   :: [Usage],
        mi_exports  :: [IfaceExport] {-,-}

--      mi_decls    :: [(Version,IfaceDecl)] {-,-}

--      mi_mod_vers :: !Version,
--      mi_orphan   :: !Bool,
--      mi_boot     :: !Bool,
--      mi_exp_vers :: !Version,
--      mi_fixities :: [(OccName,Fixity)],
--      mi_deprecs  :: [IfaceDeprec],
--      mi_insts     :: [IfaceInst],
--      mi_rules     :: [IfaceRule],
--      mi_rule_vers :: !Version,
     }

emptyIface = Iface {
        mi_package = undefined,
        mi_module  = undefined,
        mi_deps    = noDependencies,
        mi_usages  = undefined,
        mi_exports = undefined
   }

-- ---------------------------------------------------------------------
-- pretty-print an interface
--
showIface :: Iface -> String
showIface (Iface { mi_package = p, mi_module  = m, 
                   mi_deps = deps, mi_usages = us }) = 
        "interface \"" ++ p ++ "\" " ++ m ++ 
        "\n" ++ pprDeps deps  ++
        "\n" ++ (concat $ intersperse "\n" (map pprUsage us)) 
 --     "\n" ++ (concat $ intersperse "\n" (map pprExport es))

pprDeps :: Dependencies -> String
pprDeps (Deps { dep_mods = mods, dep_pkgs = pkgs }) 
        = "module dependencies: "    ++ (concat $ intersperse ", " mods) ++
          "\npackage dependencies: " ++ (concat $ intersperse ", " pkgs)

pprUsage :: Usage -> String
pprUsage usage = hsep ["import", usg_name usage]

pprExport :: IfaceExport -> String
pprExport (fsmod, items)
        = hsep [ "export", unpackFS fsmod, hsep (map pp_avail items) ]
  where
    pp_avail :: GenAvailInfo OccName -> String
    pp_avail (Avail nm)    = ppr_occ nm
    pp_avail (AvailTC _ []) = empty
    pp_avail (AvailTC n (n':ns))
        | n==n'     = (ppr_occ n) ++ pp_export ns
        | otherwise = (ppr_occ n) ++ "|" ++ pp_export (n':ns)

    pp_export []    = empty
    pp_export names = "{" ++ (hsep (map ppr_occ names)) ++ "}"

    ppr_occ (OccName _ s) = s

--
-- TODO bring in the Pretty library
--
hsep  = \ss -> concat (intersperse " " ss)
empty = ""

-- ---------------------------------------------------------------------
--
-- Dependency info about modules and packages below this one
-- in the import hierarchy.  See TcRnTypes.ImportAvails for details.
--
-- Invariant: the dependencies of a module M never includes M
-- Invariant: the lists are unordered, with no duplicates
--
-- The fields are:
--      Home-package module dependencies
--      External package dependencies
--      Orphan modules (whether home or external pkg)

data Dependencies = Deps { 
        dep_mods  :: [ModuleName],
        dep_pkgs  :: [PackageName] {-,-} 
     } deriving (Show)

noDependencies :: Dependencies
noDependencies = Deps [] []

--
-- Type aliases need to have a real type so the parser can work out how
-- to parse them. You have to find what these are by reading GHC.
--
type ModuleName   = String     {- was FastString -}      -- Module
type PackageName  = String     {- was FastString -}      -- Packages
type Version      = Int                                  -- BasicTypes
type EncodedFS    = FastString                           -- FastString
type IfaceExport  = (EncodedFS, [GenAvailInfo OccName])  -- HscTypes

data GenAvailInfo name  
        = Avail   name   -- An ordinary identifier
        | AvailTC name   -- The name of the type or class
                 [name]  -- The available pieces of type/class.
                         -- NB: If the type or class is itself
                         -- to be in scope, it must be in this list.
                         -- Thus, typically: AvailTC Eq [Eq, ==, /=]
        deriving Show

data OccName      = OccName NameSpace String {- was EncodedFS -}
        deriving Show

instance Eq OccName where
        (OccName sp1 s1) == (OccName sp2 s2) = s1 == s2 && sp1 == sp2

data NameSpace    = VarName     -- variables, and "source" data constructors
                  | DataName    -- "real" data constructors
                  | TvName      -- tyvars
                  | TcClsName   -- type constructors and classes
        deriving (Eq, Show)

data Usage
  = Usage { usg_name     :: ModuleName,		-- Name of the module
	    usg_mod      :: Version,		-- Module version
	    usg_exports  :: Maybe Version,	-- Export-list version, if we depend on it
	    usg_entities :: [(OccName,Version)],-- Sorted by occurrence name
	    usg_rules    :: Version 		-- Orphan-rules version (for non-orphan
						-- modules this will always be initialVersion)
    } deriving Show

------------------------------------------------------------------------
-- TODO parsing type and decl information out of the .hi file
-- complex data structure...
--

{-
data IfaceExtName
  = ExtPkg ModuleName OccName		-- From an external package; no version #
					-- Also used for wired-in things regardless
					-- of whether they are home-pkg or not

  | HomePkg ModuleName OccName Version	-- From another module in home package;
					-- has version #

  | LocalTop OccName			-- Top-level from the same module as 
					-- the enclosing IfaceDecl

  | LocalTopSub		-- Same as LocalTop, but for a class method or constr
	OccName		-- Class-meth/constr name
	OccName		-- Parent class/datatype name
	-- LocalTopSub is written into iface files as LocalTop; the parent 
	-- info is only used when computing version information in MkIface

data IfaceTyCon 	-- Abbreviations for common tycons with known names
  = IfaceTc IfaceExtName	-- The common case
  | IfaceIntTc | IfaceBoolTc | IfaceCharTc
  | IfaceListTc | IfacePArrTc
  | IfaceTupTc Boxity Arity 

type Arity        = Int                                 -- BasicTypes

data Boxity
  = Boxed
  | Unboxed

type IfaceContext = [IfacePredType]

data IfacePredType 	-- NewTypes are handled as ordinary TyConApps
  = IfaceClassP IfaceExtName [IfaceType]
  | IfaceIParam (IPName OccName) IfaceType

data IPName name
  = Dupable   name	-- ?x: you can freely duplicate this implicit parameter
  | Linear name		-- %x: you must use the splitting function to duplicate it
  deriving( Eq, Ord )	-- Ord is used in the IP name cache finite map
			--	(used in HscTypes.OrigIParamCache)

data IfaceType
  = IfaceTyVar    OccName		-- Type variable only, not tycon
  | IfaceAppTy    IfaceType IfaceType
  | IfaceForAllTy IfaceTvBndr IfaceType
  | IfacePredTy IfacePredType
  | IfaceTyConApp IfaceTyCon [IfaceType]	-- Not necessarily saturated
						-- Includes newtypes, synonyms, tuples
  | IfaceFunTy  IfaceType IfaceType

data IfaceBndr 		-- Local (non-top-level) binders
  = IfaceIdBndr IfaceIdBndr
  | IfaceTvBndr IfaceTvBndr

type IfaceIdBndr  = (OccName, IfaceType)	-- OccName, because always local
type IfaceTvBndr  = (OccName, IfaceKind)
type IfaceKind    = Kind			-- Re-use the Kind type, but no KindVars in it

data IfaceIdInfo
  = NoInfo			-- When writing interface file without -O
  | HasInfo [IfaceInfoItem]	-- Has info, and here it is

data IfaceInfoItem
  = HsArity	 Arity
  | HsStrictness StrictSig
  | HsUnfold	 Activation IfaceExpr
  | HsNoCafRefs
  | HsWorker	 IfaceExtName Arity	-- Worker, if any see IdInfo.WorkerInfo
					-- for why we want arity here.
	-- NB: we need IfaceExtName (not just OccName) because the worker
	--     can simplify to a function in another module.
-- NB: Specialisations and rules come in separately and are
-- only later attached to the Id.  Partial reason: some are orphans.

newtype StrictSig = StrictSig DmdType

data IfaceDecl 
  = IfaceId { ifName   :: OccName,
	      ifType   :: IfaceType, 
	      ifIdInfo :: IfaceIdInfo }

  | IfaceData { ifCtxt     :: IfaceContext,	-- Context
		ifName     :: OccName,		-- Type constructor
		ifTyVars   :: [IfaceTvBndr],	-- Type variables
		ifCons	   :: IfaceConDecls,	-- Includes new/data info
	        ifRec	   :: RecFlag,		-- Recursive or not?
		ifVrcs     :: ArgVrcs,
		ifGeneric  :: Bool		-- True <=> generic converter functions available
    }						-- We need this for imported data decls, since the
						-- imported modules may have been compiled with
						-- different flags to the current compilation unit

  | IfaceSyn  {	ifName   :: OccName,		-- Type constructor
		ifTyVars :: [IfaceTvBndr],	-- Type variables
		ifVrcs   :: ArgVrcs,
		ifSynRhs :: IfaceType		-- synonym expansion
    }

  | IfaceClass { ifCtxt    :: IfaceContext, 	 	-- Context...
		 ifName    :: OccName,		    	-- Name of the class
		 ifTyVars  :: [IfaceTvBndr],		-- Type variables
		 ifFDs     :: [FunDep OccName],		-- Functional dependencies
		 ifSigs    :: [IfaceClassOp],		-- Method signatures
	         ifRec	   :: RecFlag,			-- Is newtype/datatype associated with the class recursive?
		 ifVrcs    :: ArgVrcs			-- ... and what are its argument variances ...
    }

  | IfaceForeign { ifName :: OccName,			-- Needs expanding when we move beyond .NET
		   ifExtName :: Maybe FastString }
-}

------------------------------------------------------------------------
--
-- all this stuff may be enabled if we ever want other information out
--

{-
type ArgVrcs      = [(Bool,Bool)]                       -- TyCon
type CLabelString = FastString                          -- CStrings
type CcName       = EncodedFS                           -- CostCentre
type DeprecTxt    = FastString                          -- BasicTypes
type FunDep a     = ([a],[a])                           -- Class
type IfaceAlt     = (IfaceConAlt,[OccName],IfaceExpr)   -- IfaceSyn
type IfaceContext = [IfacePredType]                     -- IfaceType
type IfaceDeprec  = Deprecs [(OccName,DeprecTxt)]       -- HscTypes
type IfaceIdBndr  = (OccName, IfaceType)                -- IfaceType
type IfaceKind    = Kind                                -- IfaceType
type IfaceTvBndr  = (OccName, IfaceKind)                -- IfaceType
type RuleName     = FastString                          -- CoreSyn

--
-- Empty definitions for the various types we need, but whose results we
-- don't care about.
--
-- 'data' types that have a parsing method associated with them
-- This list corresponds to each instance in BinIface
--
-- Try to keep this list ordered by the order they appear in BinIface
--
data Deprecs a          = Deprecs 
data Activation         = Activation 
data StrictnessMark     = StrictnessMark
data Boxity             = Boxity 
data TupCon             = TupCon
data RecFlag            = RecFlag
data DefMeth            = DefMeth
data FixityDirection    = FixityDirection
data Fixity             = Fixity
data DmdType            = DmdType
data Demand             = Demand
data Demands            = Demands
data DmdResult          = DmdResult
data StrictSig          = StrictSig
data IsCafCC            = IsCafCC
data IsDupdCC           = IsDupdCC
data CostCentre         = CostCentre
data IfaceExtName       = IfaceExtName
data IfaceBndr          = IfaceBndr
data Kind               = Kind
data IfaceTyCon         = IfaceTyCon
data IfacePredType      = IfacePredType
data IfaceExpr          = IfaceExpr
data IfaceConAlt        = IfaceConAlt
data IfaceBinding       = IfaceBinding
data IfaceIdInfo        = IfaceIdInfo
data IfaceNoteItem      = IfaceNoteItem
data IfaceInfoItem      = IfaceInfoItem
data IfaceNote          = IfaceNote
data IfaceInst          = IfaceInst
data IfaceConDecls      = IfaceConDecls
data IfaceConDecl       = IfaceConDecl
data IfaceClassOp       = IfaceClassOp
data IfaceRule          = IfaceRule
data Literal            = Literal
data ForeignCall        = ForeignCall
data Safety             = Safety
data CExportSpec        = CExportSpec
data CCallSpec          = CCallSpec
data CCallTarget        = CCallTarget
data CCallConv          = CCallConv
data DNCallSpec         = DNCallSpec
data DNKind             = DNKind
data DNType             = DNType

-}
