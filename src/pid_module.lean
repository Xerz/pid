import linear_algebra.free_module

section pid_module

variables {ι : Type*} {R : Type*} [integral_domain R] [is_principal_ideal_ring R]
variables {M  : Type*} [add_comm_group M] [module R M] {b : ι → M}

-- Theorem 2.10 from Conrad (page 5)
theorem theorem_2_10
  (n m : ℕ)
  (bM : fin m → M)
  (freeM : is_basis R bM) -- M is a free R-module of rank m
  (N : submodule R M)
  (bN : fin n → N)
  (freeN : is_basis R bN) -- N is a submodule of M of rank n
  (rank_le : n ≤ m)       -- of smaller rank (that is always true)
  (nonzero : 0 < n)       -- N is nonzero
   : ∃ (bM' : fin m → M)  -- there exists a basis of M
       (bN' : fin n → N)  -- there exists a basis of N
       (a : fin n → R),   -- and a list of coefficients
      is_basis R bN' ∧    -- bN' is a basis of N
      ∀ i : fin n, ↑(bN' i) = a i • bM' (fin.cast_le rank_le i) ∧ -- s.t. (bN' i) is a scalar multiple of (bN i) 
      ∀ i : fin n.pred, a (fin.cast_le (nat.pred_le n) i) ∣
                       a (fin.cast (nat.succ_pred_eq_of_pos nonzero) i.succ) -- a i divides a (i+1) for all i < n
        :=
begin
  sorry,
end

end pid_module
