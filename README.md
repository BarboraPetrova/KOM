# Data Compression Course (KOM) – Homework Repository

## Implemented Algorithms
* Burrows–Wheeler Transform (BWT) and its inverse
* Run-Length Encoding (RLE)
* Move-To-Front Transform (MTF)

## Structure
- `app/Algo`: Contains core compression algorithms:
    - `BWT.hs`: Burrows–Wheeler Transform and inverse
    - `MTF.hs`: Move-To-Front transform and inverse
    - `RLE.hs`: Run-Length Encoding and decoding
- `app/Pipeline.hs` : Connects the algorithms into a full compression/decompression pipeline (e.g. BWT → MTF → RLE and reverse process)
- `app/Main.hs`

## What the Program Does

1. Reads input (from stdin or a file)
2. Encodes it using the Burrows–Wheeler Transform:

   ```haskell
   (encoded, idx) = bwtEncode input
   ```
3. Decodes it back:

   ```haskell
   decoded = toString $ bwtDecode (encoded, idx)
   ```

## Expected Behavior

* The program prints:

  * Encoded data (list of bytes + index)
  * Decoded output (original input)

* The transformation is **lossless**:

```haskell
bwtDecode (bwtEncode x) == x
```

## Example

Input:

```
banana
```

Possible output:

```
Encoded:
[(110,1),(0,1),(99,2),(0,2)]
3

Decoded:
"banana"
```

### Explanation

* The encoded list is the transformed data (BWT last column)
* The number `3` is the index of the original string in the sorted rotation table
* The decoded output reconstructs the original input exactly


## How to Run

This project uses **Cabal** to build and run the program.


### 1. Build the project

```bash
cabal build
```


### 2. Run the program

You can run it in two ways:


### Option A: Input from terminal (stdin)

```bash
cabal run
```

Then type input and finish with:

* `Ctrl+D` (Linux/macOS)
* `Ctrl+Z` (Windows)

Example:

```
$ cabal run
banana
Encoded:
[(110,1),(0,1),(99,2),(0,2)]
3

Decoded:
"banana"
```


### Option B: Input from file

```bash
cabal run -- input.txt
```

Example:

```bash
$ echo "banana" > input.txt
$ cabal run -- input.txt
```

Output:

```
Encoded:
[(110,1),(0,1),(99,2),(0,2)]
3

Decoded:
"banana"
```

