route: home
title: Home

---

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
stack build
```


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
route: <the route of the page>

---

<The rest of the post in Markdown format>
```

## Home page

The front page will either be `Home` or, if that isn't available, the blog page will be the front page.
