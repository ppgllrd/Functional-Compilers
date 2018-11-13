module FreeVars (freeVars) where

import CoreLang
import LibSet

freeVars :: CoreProgram -> AnnProgram Name (Set Name)
freeVars prog = [ (name, args, freeVars_e (setFromList args) body)
                | (name, args, body) <- prog
                ]


freeVars_e :: (Set Name)                   -- Candidates for free variables
              -> CoreExpr                  -- Expression to annotate
              -> AnnExpr Name (Set Name)   -- Annotated result
freeVars_e lv (ENum k)      = (setEmpty, ANum k)
freeVars_e lv (EVar v) | setElementOf v lv = (setSingleton v, AVar v)
                       | otherwise         = (setEmpty, AVar v)
freeVars_e lv (EAp e1 e2)
 = (setUnion (freeVarsOf e1') (freeVarsOf e2'), AAp e1' e2')
   where e1'            = freeVars_e lv e1
         e2'            = freeVars_e lv e2
freeVars_e lv (ELam args body)
 = (setSubtraction (freeVarsOf body') (setFromList args), ALam args body')
   where body'          = freeVars_e new_lv body
         new_lv         = setUnion lv (setFromList args)
freeVars_e lv (ELet is_rec defns body)
 = (setUnion defnsFree bodyFree, ALet is_rec defns' body')
   where binders        = bindersOf defns
         binderSet      = setFromList binders
         body_lv        = setUnion lv binderSet
         rhs_lv | is_rec    = body_lv
                | otherwise = lv

         rhss'          = map (freeVars_e rhs_lv) (rhssOf defns)
         defns'         = zip binders rhss'
         freeInValues   = setUnionLists (map freeVarsOf rhss')
         defnsFree | is_rec    = setSubtraction freeInValues binderSet
                   | otherwise = freeInValues
         body'          = freeVars_e body_lv body
         bodyFree       = setSubtraction (freeVarsOf body') binderSet
freeVars_e lv (ECase e alts)  = freeVars_case lv e alts
freeVars_e lv (EConstr t a n) = (setEmpty, AConstr t a n)


freeVarsOf :: AnnExpr Name (Set Name) -> Set Name
freeVarsOf (free_vars, expr) = free_vars
freeVarsOf_alt :: AnnAlt Name (Set Name) -> Set Name
freeVarsOf_alt (tag, args, rhs)
 = setSubtraction (freeVarsOf rhs) (setFromList args)

freeVars_case lv e alts
  = (setUnion (freeVarsOf e') free, ACase e' alts')
    where 
    e'     = freeVars_e lv e
    alts'  = [ (tag, args, freeVars_e (setUnion lv (setFromList args)) e) 
             | (tag, args, e) <- alts]
    free   = setUnionLists (map freeVarsOf_alt alts')

