route: about2.md
title: About

---

Hablog
======

A simple static blog platform with tags. Made with Haskell and Scotty.

Hablog will read posts written in Markdown from the `_posts` folder.

License
=======

Hablog is licensed under MIT license. This means the Haskell source files in the src directory.
Highlight.js related content is not a part of Hablog and is not licensed by it.


Installation
============

```sh
git clone https://github.com/soupi/hablog
cd hablog
cabal sandbox init
cabal install
```


How to write a new post?
========================

1. All posts must go under the `/_posts/` directory
2. The file name of each post must be `yyyy-mm-dd-<file-name-seperated-by-dashes>.md`
3. The content of the post must correspond to a specific structure

## A Post's Structure

```markdown
title: <the title of the post>
authors: <the author of the post, seperated, by, commas>
tags: <tags for the post, separated, by, commas>

[...blank line...]

<The rest of the post in Markdown format>
```

