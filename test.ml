(*Main file in which we call the module test files form *)
open OUnit2

let suite = "test suite for Wizard" >::: List.flatten []

let _ = run_test_tt_main suite
