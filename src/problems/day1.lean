import data.buffer.parser
import system.io
import category.basic

namespace day1

def fn := "input/day1.txt"

open parser

def sign : parser (ℤ → ℤ) := id <$ ch '+' <|> has_neg.neg <$ ch '-'

def digit : parser ℤ := (λ c, char.to_nat c - '0'.to_nat) <$> sat (∈ "0123456789".to_list)

def parse_nat : parser ℤ :=
do ds ← many1 digit,
   pure $ ds.foldl (λ acc d, acc * 10 + d) 0

def num : parser ℤ := sign <*> parse_nat
def WS  :=  () <$ many (sat char.is_whitespace)
def parse_all_the_stuff : parser (list ℤ) := many (num <* WS)

def pp_i (i : ℤ) : string :=
if i > 0 then "+" ++ to_string i
         else to_string i

def sum.to_io {α} : string ⊕ α → io α
| (sum.inl e) := monad_fail.fail $ "\n" ++ e
| (sum.inr r) := pure r

instance {α} : has_coe (string ⊕ α) (io α) :=
⟨ sum.to_io ⟩

def main : io unit :=
do bs ← io.fs.read_file fn,
   r ← parser.run parse_all_the_stuff bs,
   io.print $ sformat!"day1:\n{r.foldl (+) 0}"

end day1
