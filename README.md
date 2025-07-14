# wordvec
OCaml implementation of word embedding algorithms with minimal dependencies.

Supported algorithms:
✅️ Word2Vec

Steps
```bash
brew install opam

dune build
# Add extracted XML file in data/train/data.xml
dune exec bin/train.exe
# Evaluate Top 5 similar words for common words
dune exec bin/evaluate.exe
# Evaluate against WordSim-353 dataset
dune exec bin/benchmark.exe
```
