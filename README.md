# wordvec
OCaml implementation of word embedding algorithms with minimal dependencies.

Supported algorithms:
- word2vec (SkipGram)

## Getting started

For a reasonably large example corpus, I would recommend using Simple Wikipedia dumps available at https://dumps.wikimedia.org/simplewiki/.

Training on the English Simple Wikipedia corpus (around 1.3GB in XML) results in Pearson Correlation of 0.50 with Wordsim-353 benchmark.

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


