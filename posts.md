---
title: Posts
---

My collection of thoughts on various topics. Some would call this a "blog", however I promise neither regularity of updates nor coherence of writing.

<ul>
{% for post in site.posts %}
  <li><time datetime="{{ post.date }}">{{ post.date | date: "%B %-d, %Y" }}</time>: <a href="{{ post.url }}">{{ post.title }}</a></li>
{% endfor %}
</ul>
