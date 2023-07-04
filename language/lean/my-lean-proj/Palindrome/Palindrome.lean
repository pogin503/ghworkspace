inductive Palindrome : List α -> Prop where
 | nil  : Palindrome []
 | single : (a : α) -> Palindrome [a]
 | sandwich : (a : α) -> Palindrome as -> Palindrome ([a] ++ as ++ [a])

theorem palindrome_reverse (h : Palindrome as) : Palindrome as.reverse := by 
  induction h with
  | nil => exact Palindrome.nil
  | single a => exact Palindrome.single a
  | sandwich a h ih => simp; exact Palindrome.sandwich _ ih
  