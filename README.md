# The Messenger or the Message?
## The Drivers of Quote Engagement on Goodreads

**Summary of findings:** Author identity alone explains 53% of the variance in quote engagement on Goodreads. After controlling for authorial reputation via fixed effects, textual content explains a further 7.9 percentage points of within-author variation; and 89% of that content signal is carried by deep semantic embeddings, not surface-level stylistic features. Sentiment score contributes less than 1%. The implication is that (on this platform) literary engagement is primarily a reputation market, and when content does matter, it is about thematic substance rather than how something is written.

---

Regressing quote engagement on content features without accounting for author identity produces estimates that are almost certainly biased. Quotes from well-known authors are more likely to be seen, more likely to be tagged, and more likely to be liked regardless of what is actually written. Any apparent correlation between textual properties and engagement may very well simply reflect the shadow of authorial fame rather than anything intrinsic to the quote.

The question begged to be asked is more precise: **conditional on who wrote it, does what was written still matter?**

This project addresses that question through a series of structured stages. First, I estimate the share of engagement variance attributable to author identity alone (the *messenger effect*) using a saturated author fixed-effects model. Second, I test whether textual content features explain meaningful additional within-author variation across three nested model specifications (the *message effect*). Third, I apply SHAP-based feature attribution to a gradient-boosted model to decompose the content signal across feature families: structural properties, sentiment, semantic embeddings, and topic clusters.

The dataset is the [Goodreads Quotes dataset](https://www.kaggle.com/akmittal/goodreads-quotes) from Kaggle, containing 2,996 quotes across 1,409 unique authors with associated like counts and user-assigned tags.

This project can also be found on my [personal website](jordan.amurillo.workers.dev/).