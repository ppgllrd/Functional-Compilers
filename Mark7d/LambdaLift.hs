module LambdaLift (lambdaLift) where

import CoreLang
import Names
import FreeVars
import Abstract
import LibAssoc
import CoreParser
import CoreLexer
import List

lambdaLift :: CoreProgram -> CoreProgram


rename :: CoreProgram -> CoreProgram
collectSCs :: CoreProgram -> CoreProgram

lambdaLift = collectSCs . rename . abstract . freeVars

runS = pprint . lambdaLift . parser  . lexer


rename_e :: Assoc Name Name                   -- Binds old names to new
            -> NameSupply                     -- Name supply
            -> CoreExpr                       -- Input expression
            -> (NameSupply, CoreExpr)         -- Depleted supply and result
rename prog
 = snd (mapAccumL rename_sc initialNameSupply prog)
   where
   rename_sc ns (sc_name, args, rhs)
    = (ns2, (sc_name, args', rhs'))
      where
      (ns1, args', env) = newNames ns args
      (ns2, rhs') = rename_e env ns1 rhs
newNames :: NameSupply -> [Name] -> (NameSupply, [Name], Assoc Name Name)
newNames ns old_names
 = (ns', new_names, env)
   where
   (ns', new_names) = getNames ns old_names
   env = aFromList (zip old_names new_names)
rename_e env ns (EVar v)        = (ns, EVar (aLookupWithDefault env v v))
rename_e env ns (ENum n)        = (ns, ENum n)
rename_e env ns (EConstr t a n) = (ns, EConstr t a n)
rename_e env ns (EAp e1 e2)
 = (ns2, EAp e1' e2')
   where
   (ns1, e1') = rename_e env ns e1
   (ns2, e2') = rename_e env ns1 e2
rename_e env ns (ELam args body)
 = (ns2, ELam args' body')
   where
   (ns1, args', env') = newNames ns args
   (ns2, body') = rename_e (env `aUnion` env') ns1 body
rename_e env ns (ELet is_rec defns body)
 = (ns3, ELet is_rec (zip binders' rhss') body')
   where
   (ns1, body') = rename_e body_env ns body
   binders = bindersOf defns
   (ns2, binders', env') = newNames ns1 binders
   body_env = env `aUnion` env'
   (ns3, rhss') = mapAccumL (rename_e rhsEnv) ns2 (rhssOf defns)
   rhsEnv | is_rec    = body_env
          | otherwise = env
rename_e env ns (ECase e alts) = rename_case env ns e alts
collectSCs_e :: CoreExpr -> ([CoreScDefn], CoreExpr)
collectSCs prog
 = concat (map collect_one_sc prog)
   where
   collect_one_sc (sc_name, args, rhs)
    = (sc_name, args, rhs') : scs
     where
     (scs, rhs') = collectSCs_e rhs
collectSCs_e (ENum k)      = ([], ENum k)
collectSCs_e (EVar v)      = ([], EVar v)
collectSCs_e (EAp e1 e2)   = (scs1 ++ scs2, EAp e1' e2')
                             where
                             (scs1, e1') = collectSCs_e e1
                             (scs2, e2') = collectSCs_e e2
collectSCs_e (ELam args body) = (scs, ELam args body')
                                where
                                (scs, body') = collectSCs_e body
collectSCs_e (EConstr t a n) = ([], EConstr t a n)
collectSCs_e (ECase e alts)
 = (scs_e ++ scs_alts, ECase e' alts')
   where
   (scs_e, e') = collectSCs_e e
   (scs_alts, alts') = mapAccumL collectSCs_alt [] alts
   collectSCs_alt scs (tag, args, rhs) = (scs++scs_rhs, (tag, args, rhs'))
                                         where
                                         (scs_rhs, rhs') = collectSCs_e rhs
collectSCs_e (ELet is_rec defns body)
 = (rhss_scs ++ body_scs ++ local_scs, mkELet is_rec non_scs' body')
   where
   (rhss_scs,defns') = mapAccumL collectSCs_d [] defns

   scs'     = [(name,rhs) | (name,rhs) <- defns',  isELam rhs ]
   non_scs' = [(name,rhs) | (name,rhs) <- defns',  not (isELam rhs)]
   local_scs = [(name,args,body) | (name,ELam args body) <- scs']

   (body_scs, body') = collectSCs_e body

   collectSCs_d scs (name,rhs) = (scs ++ rhs_scs, (name, rhs'))
                                 where
                                 (rhs_scs, rhs') = collectSCs_e rhs
isELam :: Expr a -> Bool
isELam (ELam args body) = True
isELam other            = False
mkELet is_rec defns body = ELet is_rec defns body   



mkELet is_rec [] e    = e
mkELet is_rec defns e = ELet is_rec defns e



rename_case env ns e alts
  = (ns2, ECase e' alts')
    where
    (ns1, e') = rename_e env ns e
    (ns2, alts') = mapAccumL rename_alt ns alts
    rename_alt ns (tag, args, rhs)
     = (ns2, (tag, args', rhs'))
       where
       (ns1, args', env') = newNames ns args
       (ns2, rhs')  = rename_e (env' `aUnion` env) ns1 rhs
