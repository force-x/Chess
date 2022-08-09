(** CS 3110 Spring 2022 MS3: Release

    @author Logan Knapp (lhk62), David Forcinito (dvf26), Omar Elhosseni (oe36) *)

(************************************************************

   Academic Integrity Statement

   I, the person named in the author comment above, have fully reviewed the
   course academic integrity policies.  I have adhered to those policies in
   solving the assignment.

   The policies do permit some limited collaboration among students currently
   enrolled in the course. If I did engage in such collaboration, here is the
   list of other students with whom I collaborated, and a brief summary of that
   collaboration:

   - Logan Knapp (lhk62)
   - David Forcinito (dvf26)
   - Omar Elhosseni (oe36)

 ************************************************************)

type t = {
  david : int;
  logan : int;
  omar : int;
}

val hours_worked : t

(** [hours_worked] is the number of hours each of us worked during the
    assignment. *)
