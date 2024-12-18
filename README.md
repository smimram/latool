# LaTool -- preprocess LaTeX files

A tool to preprocess LaTeX files. Most interestingly it can perform **grammar checks** on LaTeX files.

The following options are currently supported:

- `--expand`: replace `\input` commands by their content
- `--grammar`: check for spelling mistakes and grammar errors
- `--remove-comments`: remove comments

This is programmed in OCaml, improvements and suggestions are welcome.

## Proofreading using large language models

The `--grammar` flag splits the text and feeds it to a large language model. In order to for it to work, you should run a [llama.cpp](https://github.com/ggerganov/llama.cpp) server with

```sh
llama-server -m your_model.gguf
```

In practice, [Llama 3.2 3B Instruct](https://huggingface.co/meta-llama/Llama-3.2-3B) performs reasonably fast and well (download the corresponding GGUF file [here](https://huggingface.co/bartowski/Llama-3.2-3B-Instruct-GGUF)). Note that, in this way, your file is never disclosed to distant servers. The output should be a (roughly valid) markdown file containing the text as well as suggested changes. In order to speed up the process, some LaTeX markup is removed from the input.
