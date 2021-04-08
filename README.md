Hablog
======

[![Hackage](https://img.shields.io/hackage/v/hablog.svg)](http://hackage.haskell.org/package/hablog)

A simple blog platform with tags. Made with Haskell and Scotty.

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
stack build
```


Running
=======

```
hablog --title <TITLE> --theme <THEME> --domain <DOMAIN> <COMMAND> [--port <PORT> --tls-cert <TLS_CERT> --tls-port <TLS_PORT>]
```

- `<TITLE>` is the title you want in the HTML headers;
- `<THEME>` is `light` or `dark`, depending on the theme you want, to create your own themes look at the examples in [/static/css](https://github.com/soupi/hablog/tree/master/static/css);
- `<DOMAIN>` is the domain you're running the blog on;
- `<COMMAND>` is `http`, `https`, or `both`;
- `<PORT>` is the `http` port (not required if `COMMAND` is `https`);
- `<TLS_CERT>` is the `https` certificate (not required if `COMMAND` is `http`);
- `<TLS_PORT>` is the `https` port (not required if `COMMAND` is `http`).


How to write a new post?
========================

1. All posts must go under the `/_posts/` directory
1. All pages must go under the `/_pages/` directory
3. The content of the post/page must correspond to a specific structure

## A Post's Structure

```markdown
title: <the title of the post>
route: <route to the post>
authors: <the author of the post, seperated, by, commas>
date: yyyy-mm-dd
tags: <tags for the post, separated, by, commas>

---

<The rest of the post in Markdown format>
```


## A Page's Structure

```markdown
title: <the title of the page>
route: <route to the page>
---

<The rest of the page in Markdown format>
```

## Preview

If you want twitter/other previews to work as well, add the following definition at the header of a post or page. Not that `image` without `image-alt` won't work.

- `summary` - For a summary of the post/page
- `image` - A link to an image
- `image-alt` - Alternative description for an image
