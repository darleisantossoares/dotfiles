heat Sheet

## Basic Editing Commands
- **Open or Switch Buffer:** `C-x b`
- **Save File:** `C-x C-s`
- **Cut (Kill) Line:** `C-k`
- **Copy Line:** `M-w` (after selecting text)
- **Paste (Yank):** `C-y`
- **Undo:** `C-/` or `C-x u`
- **Search (Incremental):** `C-s` (forward), `C-r` (backward)
- **Search (Incremental with Regex):** `C-M-s` (forward), `C-M-r` (backward)
- **Replace:** `M-%`
- **Replace with Regex:** `C-M-%`
- **Go to Line:** `M-g g`
- **Open New Line Below:** `C-o`
- **Kill (Close) Current Buffer:** `C-x k`
- **Close Buffer (Alternative):** `C-x C-k`

## Buffer Navigation
- **Next Buffer:** `C-x <right>`
- **Previous Buffer:** `C-x <left>`
- **Kill (Close) the Current Buffer:** `C-x k` (then press Enter if prompted)

## Paredit (for Structured Editing of Lisp S-expressions)
- **Wrap Round:** `M-(` - Wrap the following expression in parentheses
- **Slurp Forward:** `C-c >` - Extend the current s-expression forward
- **Barf Forward:** `C-c <` - Shrink the current s-expression forward
- **Slurp Backward:** `C-c M->` - Extend the current s-expression backward
- **Barf Backward:** `C-c M-<` - Shrink the current s-expression backward
- **Kill S-expression:** `C-M-k` - Kill the next s-expression
- **Splice S-expression:** `M-s` - Remove the enclosing parentheses

## CIDER (Clojure Interactive Development Environment that Rocks)
- **Start/Connect to REPL:** `M-x cider-jack-in` or `M-x cider-connect`
- **Evaluate Buffer:** `C-c C-k` - Evaluate the current buffer
- **Evaluate Defun at Point:** `C-c C-c` - Evaluate top-level s-expression around point
- **Load Buffer and Switch to REPL Buffer:** `C-c C-z`
- **Show Documentation for Symbol at Point:** `C-c C-d C-d`
- **Go to Function/Var Definition:** `C-c d` - Jump to the definition of the symbol at point
- **Find Function References:** `C-c r` - Find all references to the function at point

## Projectile (Project Interaction Library for Emacs)
- **Find File in Project:** `C-c p f`
- **Switch to Buffer:** `C-c p b`
- **Search in Project with Grep:** `C-c p s g`
- **Run a Command in the Root of the Project:** `C-c p !`
- **Invalidate the Project Cache:** `C-c p i`
- **Add Known Project:** `C-c p a`
- **Switch to Known Project:** `C-c p p`

## Window Management
- **Split Window Vertically:** `C-x |` (New window to the right)
- **Split Window Horizontally:** `C-x -` (New window below)
- **Close Current Window:** `C-x 0`
- **Close Other Windows:** `C-x 1`

## Navigation Between Windows
- **Move to Left Window:** `C-w <left>`
- **Move to Right Window:** `C-w <right>`
- **Move to Upper Window:** `C-w <up>`
- **Move to Lower Window:** `C-w <down>`

