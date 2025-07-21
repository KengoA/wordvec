# wordvec
OCaml implementation of word embedding algorithms with minimal dependencies.

Supported algorithms:
- word2vec (SkipGram)

## Getting started

For a reasonably large example corpus, I would recommend using Simple Wikipedia dumps available at https://dumps.wikimedia.org/simplewiki/.

Training on the English Simple Wikipedia corpus (around 1.3GB in XML) results in Pearson Correlation of 0.50 with Wordsim-353 benchmark.

```bash
brew install opam

# Download and extract Simple Wikipedia dataset
curl -L https://dumps.wikimedia.org/simplewiki/latest/simplewiki-latest-pages-articles-multistream.xml.bz2 | bunzip2 > data/train/simplewikipedia.xml

dune build

# Train the model with custom configuration
dune exec bin/train.exe -- --input data/train/simplewikipedia.xml --dim 150 --window-size 15 --epochs 2 --workers 8

# Evaluate Top 5 similar words for common words
dune exec bin/evaluate.exe
# Evaluate against WordSim-353 dataset
dune exec bin/benchmark.exe
```

## Configuration Options

The training script supports the following command-line options:

- `--input`: Input file path (.xml or .txt)
- `--dim`: Embedding dimension - default: 150
- `--window-size`: Context window size - default: 15
- `--neg-samples`: Number of negative samples - default: 5
- `--epochs`: Number of training epochs - default: 1
- `--neg-table-size`: Negative sampling table size - default: 1000000
- `--chunk-size`: Chunk size in MB for processing - default: 100
- `--workers`: Number of parallel workers - default: 10
- `--min-freq`: Minimum vocabulary frequency - default: 10
- `--vocab-output`: Output vocabulary file - default: `data/artifacts/vocab_freq.csv`
- `--embed-output`: Output embeddings file - default: `data/artifacts/embeddings.csv`
