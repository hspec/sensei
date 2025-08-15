module Options.GHC where

import Imports


data Flag = Flag {
  type_ :: FlagType
, name :: String
} deriving (Eq, Show)


data FlagType =
    HasArgument
  | TightHasArgument
  | TightOptionalHasArgument  -- -f or -farg or -f=arg
  | SeparateArgument
  deriving (Eq, Show)

  -- NoArg          -f
  -- PassFlag       -f


takeGhc :: [String] -> ([String], [String])
takeGhc input = case input of
  flag : args | Just (type_, value) <- foo_ flag flags_ -> case type_ of
    HasArgument -> case dropEq value of
      "" | x : xs <- args -> ([flag, x], xs)
      _ -> ([flag], args)
    TightHasArgument | (not . null) (dropEq value) -> ([flag], args)

    TightOptionalHasArgument -> ([flag], args)

    SeparateArgument | null value, x : xs <- args -> ([flag, x], xs)
    _ -> ([], input)



  flag : args | flag `elem` flags -> ([flag], args)
  args -> ([], args)
  where
    flags_ :: [Flag]
    flags_ =
         hasArg
      ++ map (Flag SeparateArgument) sepArg
      ++ map (Flag TightHasArgument) prefix
      ++ map (Flag TightOptionalHasArgument) optPrefix

dropEq :: String -> String
dropEq ('=' : s) = s
dropEq s         = s

foo_ :: String -> [Flag] -> Maybe (FlagType, String)
foo_ arg = mapMaybe (parseFlag arg) >>> sortOn (length . snd) >>> \ case
  [] -> Nothing
  flag : _ -> Just flag


parseFlag :: String -> Flag -> Maybe (FlagType, String)
parseFlag arg flag = (,) flag.type_ . strip <$> flag.name `stripPrefix` arg

flags :: [String]
flags = xflags ++ warningFlags ++ fflags <> dflags <> assorted


assorted :: [String]
assorted = [
  "-clear-package-db",
  "-cpp",
  "-ddisable-js-c-sources",
  "-ddisable-js-minifier",
  "-distrust-all-packages",
  "-eventlog",
  "-F",
  "-fbyte-code",
  "-fdiagnostics-color=always",
  "-fdiagnostics-color=auto",
  "-fdiagnostics-color=never",
  "-ffloat-all-lams",
  "-fno-code",
  "-fno-liberate-case-threshold",
  "-fno-max-errors",
  "-fno-max-refinement-hole-fits",
  "-fno-max-relevant-binds",
  "-fno-max-valid-hole-fits",
  "-fno-refinement-level-hole-fits",
  "-fno-safe-haskell",
  "-fno-safe-infer",
  "-fno-spec-constr-count",
  "-fno-spec-constr-threshold",
  "-fno-write-interface",
  "-fobject-code",
  "-fpackage-trust",
  "-fprint-error-index-links=always",
  "-fprint-error-index-links=auto",
  "-fprint-error-index-links=never",
  "-fstg-lift-lams-non-rec-args-any",
  "-fstg-lift-lams-rec-args-any",
  "-fwrite-interface",
  "-global-package-db",
  "-hide-all-packages",
  "-hide-all-plugin-packages",
  "-ignore-dot-ghci",
  "-n",
  "-no-global-package-db",
  "-no-ignore-dot-ghci",
  "-no-user-package-conf",
  "-no-user-package-db",
  "-pgmc-supports-no-pie",
  "-pgml-supports-no-pie",
  "-Rghc-timing",
  "-user-package-db",
  "-w"
  ]


xflags :: [String]
xflags = "-XSafe" : "-XUnsafe" : "-XTrustworthy" : map ("-X" <>) language ++ (concatMap f $ extensions ++ others)
  where
    -- f name = ["X" <> name, "XNo" <> name]
    f name = ["-X" <> name, "-XNo" <> name]

warningFlags :: [String]
warningFlags =

  -- "error=all" :
  "-W" :
  "-Werror" :
  "-Wwarn" :
  "-Wnot" :
  (concatMap f $ warnings)
  where
    -- f name = ["-W" <> name, "-Wno-" <> name]
    f name = [
        "-W" <> name
      , "-Wno-" <> name
      , "-Wwarn=" <> name
      , "-Werror=" <> name
      , "-Wno-error=" <> name
      ]

extensions :: [String]
extensions = [
  "AllowAmbiguousTypes",
  "AlternativeLayoutRule",
  "AlternativeLayoutRuleTransitional",
  "ApplicativeDo",
  "Arrows",
  "AutoDeriveTypeable",
  "BangPatterns",
  "BinaryLiterals",
  "BlockArguments",
  "CApiFFI",
  "ConstrainedClassMethods",
  "ConstraintKinds",
  "CPP",
  "CUSKs",
  "DataKinds",
  "DatatypeContexts",
  "DeepSubsumption",
  "DefaultSignatures",
  "DeriveAnyClass",
  "DeriveDataTypeable",
  "DeriveFoldable",
  "DeriveFunctor",
  "DeriveGeneric",
  "DeriveLift",
  "DeriveTraversable",
  "DerivingStrategies",
  "DerivingVia",
  "DisambiguateRecordFields",
  "DoAndIfThenElse",
  "DuplicateRecordFields",
  "EmptyCase",
  "EmptyDataDecls",
  "EmptyDataDeriving",
  "ExistentialQuantification",
  "ExplicitForAll",
  "ExplicitNamespaces",
  "ExtendedDefaultRules",
  "ExtendedLiterals",
  "FieldSelectors",
  "FlexibleContexts",
  "FlexibleInstances",
  "ForeignFunctionInterface",
  "FunctionalDependencies",
  "GADTs",
  "GADTSyntax",
  "GeneralizedNewtypeDeriving",
  "GHCForeignImportPrim",
  "HexFloatLiterals",
  "ImplicitParams",
  "ImplicitPrelude",
  "ImportQualifiedPost",
  "ImpredicativeTypes",
  "IncoherentInstances",
  "InstanceSigs",
  "InterruptibleFFI",
  "JavaScriptFFI",
  "KindSignatures",
  "LambdaCase",
  "LexicalNegation",
  "LiberalTypeSynonyms",
  "LinearTypes",
  "ListTuplePuns",
  "MagicHash",
  "MonadComprehensions",
  "MonoLocalBinds",
  "MonomorphismRestriction",
  "MultiParamTypeClasses",
  "MultiWayIf",
  "NamedFieldPuns",
  "NamedWildCards",
  "NegativeLiterals",
  "NondecreasingIndentation",
  "NPlusKPatterns",
  "NullaryTypeClasses",
  "NumDecimals",
  "NumericUnderscores",
  "OverlappingInstances",
  "OverloadedLabels",
  "OverloadedLists",
  "OverloadedRecordDot",
  "OverloadedRecordUpdate",
  "OverloadedStrings",
  "PackageImports",
  "ParallelArrays",
  "ParallelListComp",
  "PartialTypeSignatures",
  "PatternGuards",
  "PatternSynonyms",
  "PolyKinds",
  "PostfixOperators",
  "QualifiedDo",
  "QuantifiedConstraints",
  "QuasiQuotes",
  "RankNTypes",
  "RebindableSyntax",
  "RecordWildCards",
  "RecursiveDo",
  "RelaxedLayout",
  "RelaxedPolyRec",
  "RequiredTypeArguments",
  "RoleAnnotations",
  "ScopedTypeVariables",
  "StandaloneDeriving",
  "StandaloneKindSignatures",
  "StarIsType",
  "StaticPointers",
  "Strict",
  "StrictData",
  "TemplateHaskell",
  "TemplateHaskellQuotes",
  "TraditionalRecordSyntax",
  "TransformListComp",
  "TupleSections",
  "TypeAbstractions",
  "TypeApplications",
  "TypeData",
  "TypeFamilies",
  "TypeFamilyDependencies",
  "TypeInType",
  "TypeOperators",
  "TypeSynonymInstances",
  "UnboxedSums",
  "UnboxedTuples",
  "UndecidableInstances",
  "UndecidableSuperClasses",
  "UnicodeSyntax",
  "UnliftedDatatypes",
  "UnliftedFFITypes",
  "UnliftedNewtypes",
  "ViewPatterns"
  ]

others :: [String]
others = [
  "DoRec",
  "GeneralisedNewtypeDeriving",
  "PatternSignatures",
  "PolymorphicComponents",
  "Rank2Types",
  "RecordPuns"
  ]

language :: [String]
language = [
  "Haskell98",
  "Haskell2010",
  "GHC2021",
  "GHC2024"
  ]

warnings :: [String]
warnings = [
  "all",
  "all-missed-specialisations",
  "all-missed-specializations",
  "alternative-layout-rule-transitional",
  "ambiguous-fields",
  "auto-orphans",
  "badly-staged-types",
  "compat",
  "compat-unqualified-imports",
  "cpp-undef",
  "data-kinds-tc",
  "default",
  "defaulted-exception-context",
  "deferred-out-of-scope-variables",
  "deferred-type-errors",
  "deprecated-flags",
  "deprecated-type-abstractions",
  "deriving-defaults",
  "deriving-typeable",
  "dodgy-exports",
  "dodgy-foreign-imports",
  "dodgy-imports",
  "duplicate-constraints",
  "duplicate-exports",
  "empty-enumerations",
  "everything",
  "extended-warnings",
  "extra",
  "forall-identifier",
  "gadt-mono-local-binds",
  "hi-shadowing",
  "identities",
  "implicit-kind-vars",
  "implicit-lift",
  "implicit-prelude",
  "implicit-rhs-quantification",
  "inaccessible-code",
  "incomplete-export-warnings",
  "incomplete-patterns",
  "incomplete-record-selectors",
  "incomplete-record-updates",
  "incomplete-uni-patterns",
  "inconsistent-flags",
  "inferred-safe-imports",
  "inline-rule-shadowing",
  "invalid-haddock",
  "loopy-superclass-solve",
  "misplaced-pragmas",
  "missed-extra-shared-lib",
  "missed-specialisations",
  "missed-specializations",
  "missing-deriving-strategies",
  "missing-exported-pattern-synonym-signatures",
  "missing-exported-signatures",
  "missing-exported-sigs",
  "missing-export-lists",
  "missing-fields",
  "missing-home-modules",
  "missing-import-lists",
  "missing-kind-signatures",
  "missing-local-signatures",
  "missing-local-sigs",
  "missing-methods",
  "missing-monadfail-instances",
  "missing-pattern-synonym-signatures",
  "missing-poly-kind-signatures",
  "missing-role-annotations",
  "missing-safe-haskell-mode",
  "missing-signatures",
  "missing-space-after-bang",
  "monomorphism-restriction",
  "name-shadowing",
  "noncanonical-monadfail-instances",
  "noncanonical-monad-instances",
  "noncanonical-monoid-instances",
  "operator-whitespace",
  "operator-whitespace-ext-conflict",
  "orphans",
  "overflowed-literals",
  "overlapping-patterns",
  "partial-fields",
  "partial-type-signatures",
  "prepositive-qualified-module",
  "redundant-bang-patterns",
  "redundant-constraints",
  "redundant-record-wildcards",
  "redundant-strictness-flags",
  "safe",
  "semigroup",
  "simplifiable-class-constraints",
  "star-binder",
  "star-is-type",
  "tabs",
  "term-variable-capture",
  "trustworthy-safe",
  "type-defaults",
  "typed-holes",
  "type-equality-out-of-scope",
  "type-equality-requires-operators",
  "unbanged-strict-patterns",
  "unicode-bidirectional-format-characters",
  "unrecognised-pragmas",
  "unrecognised-warning-flags",
  "unsafe",
  "unsupported-calling-conventions",
  "unsupported-llvm-version",
  "unticked-promoted-constructors",
  "unused-binds",
  "unused-do-bind",
  "unused-foralls",
  "unused-imports",
  "unused-local-binds",
  "unused-matches",
  "unused-packages",
  "unused-pattern-binds",
  "unused-record-wildcards",
  "unused-top-binds",
  "unused-type-patterns",
  "warnings-deprecations",
  "wrong-do-bind"
  ]

fflags :: [String]
fflags = concatMap f foo
  where
    f name = ["-f" <> name, "-fno-" <> name]

foo :: [String]
foo = [
  "abstract-refinement-hole-fits",
  "alignment-sanitisation",
  "allow-incoherent-instances",
  "allow-overlapping-instances",
  "allow-undecidable-instances",
  "arrows",
  "asm-shortcutting",
  "bang-patterns",
  "block-layout-cfg",
  "block-layout-weightless",
  "break-on-error",
  "break-on-exception",
  "break-points",
  "building-cabal-package",
  "byte-code-and-object-code",
  "call-arity",
  "case-folding",
  "case-merge",
  "catch-nonexhaustive-cases",
  "check-prim-bounds",
  "cmm-control-flow",
  "cmm-elim-common-blocks",
  "cmm-sink",
  "cmm-static-pred",
  "cmm-thread-sanitizer",
  "compact-unwind",
  "core-constant-folding",
  "cpr-anal",
  "cross-module-specialise",
  "cross-module-specialize",
  "cse",
  "defer-diagnostics",
  "defer-out-of-scope-variables",
  "defer-typed-holes",
  "defer-type-errors",
  "diagnostics-as-json",
  "diagnostics-show-caret",
  "dicts-cheap",
  "dicts-strict",
  "distinct-constructor-tables",
  "dmd-tx-dict-sel",
  "do-clever-arg-eta-expansion",
  "do-eta-reduction",
  "do-lambda-eta-expansion",
  "dump-with-ways",
  "eager-blackholing",
  "embed-manifest",
  "enable-rewrite-rules",
  "enable-th-splice-warnings",
  "error-spans",
  "excess-precision",
  "exitification",
  "expose-all-unfoldings",
  "expose-internal-symbols",
  "extended-default-rules",
  "external-dynamic-refs",
  "external-interpreter",
  "family-application-cache",
  "fast-pap-calls",
  "ffi",
  "fi",
  "float-in",
  "force-recomp",
  "full-laziness",
  "fun-to-thunk",
  "gen-manifest",
  "ghci-history",
  "ghci-leak-check",
  "ghci-sandbox",
  "glasgow-exts",
  "helpful-errors",
  "hide-source-paths",
  "hpc",
  "ignore-asserts",
  "ignore-hpc-changes",
  "ignore-interface-pragmas",
  "ignore-optim-changes",
  "implicit-import-qualified",
  "implicit-params",
  "implicit-prelude",
  "info-table-map",
  "info-table-map-with-fallback",
  "info-table-map-with-stack",
  "inline-generics",
  "inline-generics-aggressively",
  "irrefutable-tuples",
  "keep-auto-rules",
  "keep-cafs",
  "keep-going",
  "late-dmd-anal",
  "late-specialise",
  "liberate-case",
  "link-rts",
  "local-float-out",
  "local-float-out-top-level",
  "local-ghci-history",
  "loopification",
  "monomorphism-restriction",
  "no-it",
  "num-constant-folding",
  "omit-interface-pragmas",
  "omit-yields",
  "optimal-applicative-do",
  "orig-thunk-info",
  "pedantic-bottoms",
  "polymorphic-specialisation",
  "prefer-byte-code",
  "pre-inlining",
  "print-axiom-incomps",
  "print-bind-contents",
  "print-bind-result",
  "print-equality-relations",
  "print-evld-with-show",
  "print-expanded-synonyms",
  "print-explicit-coercions",
  "print-explicit-foralls",
  "print-explicit-kinds",
  "print-explicit-runtime-reps",
  "print-potential-instances",
  "print-redundant-promotion-ticks",
  "print-typechecker-elaboration",
  "print-unicode-syntax",
  "prof-cafs",
  "prof-count-entries",
  "prof-late",
  "prof-late-inline",
  "prof-late-overloaded",
  "prof-late-overloaded-calls",
  "prof-manual",
  "regs-graph",
  "regs-iterative",
  "reverse-errors",
  "rewrite-rules",
  "scoped-type-variables",
  "shared-implib",
  "show-docs-of-hole-fits",
  "show-error-context",
  "show-hole-constraints",
  "show-hole-matches-of-hole-fits",
  "show-loaded-modules",
  "show-provenance-of-hole-fits",
  "show-type-app-of-hole-fits",
  "show-type-app-vars-of-hole-fits",
  "show-type-of-hole-fits",
  "show-valid-hole-fits",
  "show-valid-substitutions",
  "show-warning-groups",
  "solve-constant-dicts",
  "sort-by-size-hole-fits",
  "sort-by-subsumption-hole-fits",
  "sort-valid-hole-fits",
  "spec-constr",
  "spec-constr-keen",
  "specialise",
  "specialise-aggressively",
  "specialise-incoherents",
  "specialize",
  "specialize-aggressively",
  "split-sections",
  "static-argument-transformation",
  "stg-cse",
  "stg-lift-lams",
  "stg-lift-lams-known",
  "strictness",
  "th",
  "unbox-small-strict-fields",
  "unbox-strict-fields",
  "unclutter-valid-hole-fits",
  "unoptimized-core-for-interpreter",
  "use-rpaths",
  "validate-ide-info",
  "version-macros",
  "whole-archive-hs-libs",
  "worker-wrapper",
  "worker-wrapper-cbv",
  "write-ide-info",
  "write-if-simplified-core"
  ]

dflags :: [String]
dflags = concatMap f dflag_
  where
    f name = ["-d" <> name, "-dno-" <> name]

dflag_ :: [String]
dflag_ = [
  "ppr-case-as-let",
  "ppr-ticks",
  "suppress-coercions",
  "suppress-coercion-types",
  "suppress-core-sizes",
  "suppress-idinfo",
  "suppress-module-prefixes",
  "suppress-stg-exts",
  "suppress-stg-free-vars",
  "suppress-stg-reps",
  "suppress-ticks",
  "suppress-timestamps",
  "suppress-type-applications",
  "suppress-type-signatures",
  "suppress-unfoldings",
  "suppress-uniques",
  "suppress-var-kinds"
  ]

hasArg :: [Flag]
hasArg = map (Flag HasArgument) [
  -- HasArg         -farg or -f=arg or -f arg
  "-#include",
  "-H",
  "-distrust",
  "-fblock-layout-weights",
  "-framework",
  "-framework-path",
  "-ghci-script",
  "-hide-package",
  "-ignore-package",
  "-interactive-print",
  "-l",
  "-optF",
  "-optL",
  "-optP",
  "-opta",
  "-optc",
  "-optcxx",
  "-opti",
  "-optl",
  "-optlas",
  "-optlc",
  "-optlm",
  "-optlo",
  "-optwindres",
  "-package",
  "-package-conf",
  "-package-db",
  "-package-env",
  "-package-id",
  "-pgmF",
  "-pgmL",
  "-pgmP",
  "-pgma",
  "-pgmar",
  "-pgmc",
  "-pgmcxx",
  "-pgmi",
  "-pgminstall_name_tool",
  "-pgml",
  "-pgmlas",
  "-pgmlc",
  "-pgmlm",
  "-pgmlo",
  "-pgmotool",
  "-pgmranlib",
  "-pgms",
  "-pgmwindres",
  "-plugin-package",
  "-plugin-package-id",
  "-syslib",
  "-trust"
  ]

sepArg :: [String]
sepArg = [
  -- SepArg         -f arg
  "-dinline-check",
  "-drule-check",
  "-instantiated-with",
  "-this-component-id"
  ]

prefix :: [String]
prefix = [
  -- Prefix         -farg or -f=arg
  "-I",
  "-L",

  -- IntSuffix      -fnum or -f=num
  "-dppr-cols",
  "-dppr-user-length",
  "-fbinary-blob-threshold",
  "-fconstraint-solver-iterations",
  "-fcontext-stack",
  "-fdmd-unbox-width",
  "-ffloat-lam-args",
  "-fghci-hist-size",
  "-fgivens-expansion-fuel",
  "-fhistory-size",
  "-fliberate-case-threshold",
  "-fmax-errors",
  "-fmax-pmcheck-iterations",
  "-fmax-pmcheck-models",
  "-fmax-refinement-hole-fits",
  "-fmax-relevant-binds",
  "-fmax-simplifier-iterations",
  "-fmax-uncovered-patterns",
  "-fmax-valid-hole-fits",
  "-fmax-worker-args",
  "-fproc-alignment",
  "-fqcs-expansion-fuel",
  "-freduction-depth",
  "-frefinement-level-hole-fits",
  "-fsimpl-tick-factor",
  "-fsimplifier-phases",
  "-fspec-constr-count",
  "-fspec-constr-recursive",
  "-fspec-constr-threshold",
  "-fstg-lift-lams-non-rec-args",
  "-fstg-lift-lams-rec-args",
  "-fstrictness-before",
  "-ftype-function-depth",
  "-funfolding-case-scaling",
  "-funfolding-case-threshold",
  "-funfolding-creation-threshold",
  "-funfolding-dict-discount",
  "-funfolding-fun-discount",
  "-funfolding-use-threshold",
  "-fwanteds-expansion-fuel",

  -- Word64Suffix   -fnum or -f=num

  -- FloatSuffix    -fnum or -f=num
  "-funfolding-keeness-factor"
  ]

optPrefix :: [String]
optPrefix = [
  -- OptPrefix      -f -farg or -f=arg
  "-i",

  -- OptIntSuffix   -f -fnum or -f=num
  "-v",

  -- AnySuffix      -f or -farg
  "-D",
  "-U"
  ]
