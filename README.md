![GitHub License](https://img.shields.io/github/license/SamuelVanie/youdotcom.el)
![Static Badge](https://img.shields.io/badge/Emacs%20-%2029.1%20-%20orange)

# youdotcom.el

A simple package to use Youdotcom search engine from Emacs.
Pretty neat alternative to eww in emacs.

*N.B: Remember that Youdotcom search is a search engine not a chatbot.*

<p align="center">
  <img alt="demo" src="./demo_2.gif">
</p>


## ❓ Why?

I wanted to use Youdotcom engine directly in Emacs, so I wrote this package.
The You API is very simple, so I thought it would be an alternative for people who want to use a search engine boosted by AI directly in Emacs.
You will no more need to filter the results by yourself on the web and directly get the results inside of Emacs without leaving it.

You can also use it as a simple web browser, but this is not the spirit of an emacs user, right ?

*N.B: Do not forget to check the pricing of the You search engine API.*


## 💾 Installation

### Straigt

```elisp
(use-package youdotcom
  :straight (youdotcom :type git :host github :repo "SamuelVanie/youdotcom.el"))
```

### Doom Emacs

```elisp
(package! youdotcom
  :recipe (:host github :repo "SamuelVanie/youdotcom.el"))
```


### MELPA

```elisp
(use-package youdotcom
  :bind ("C-c y" . youdotcom-enter))
```

## 🔑 Obtaining an API key

You need to obtain an API key from [You.com](https://api.you.com/).
Go to that website and get the Web Search API key, and the Web LLM API key is you want to use the RAG LLM model.


## 💻 Usage

You will have to set the API key in your init file:

```elisp
(setq youdotcom-search-api-key "YOUR_API_KEY")
(setq youdotcom-rag-api-key "YOUR_API_KEY")
```

Then you can use the following commands:

- `youdotcom-enter` : Will start the client and open the prompt for you to enter your query.
You can setup a keybinding for this command. (e.g. `(global-set-key (kbd "C-c y") 'youdotcom-enter)`) or use the one provided in the use-package installation process.

In the prompt, you can use the following commands:

- `/help` : Will display the help message.
- `/clear` : Will clear the buffer.
- `/quit` : Stop the search engine's session and close the buffer.
- `/search <query>` : Will start the search with the query you entered. The results will be displayed in the buffer, each results will have a description and the URL associated with it.
- `/rag <query>` : Will start the search with the query you entered using the RAG LLM model. Using AI, the results will be done by the engine on the internet, analyzed and an answer will be given to you.

*NB: You can change the number of results displayed by the search command by changing the variable `youdotcom-number-of-results` (default is 1).*

## 👊 Contributing

- If you find a bug or have an idea for an improvement, please open an issue about it.
- If you want to contribute, you can open a pull request and I will be happy to review it.
- If you want to add a new feature, please open an issue first to discuss about it.
- If you want to support me, you can star the repository.
