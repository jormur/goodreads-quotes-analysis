# Analyzing Viral Content on Goodreads: A Data Science Approach to Understanding Quote Popularity

This project applies advanced data science techniques to analyze what makes quotes go viral on Goodreads, combining traditional econometric methods with cutting-edge natural language processing. Using a dataset of popular quotes from the platform (found [here](https://www.kaggle.com/datasets/abhishekvermasg1/goodreads-quotes)), I investigated how content characteristics, emotional sentiment, and author influence affect engagement.

The analysis employs multiple methodological approaches:

- Natural Language Processing (BERT) for sentiment analysis
- Network analysis to map author relationships and influence
- Machine learning for popularity prediction
- Statistical analysis of quote characteristics

Key findings revealed that viral success isn't simply about positive messaging—quotes with complex emotional content often perform well, despite positive quotes averaging 1,744 likes compared to 939 for negative ones. Surface-level features like length and complexity showed surprisingly weak correlations with popularity. The project identified 8 distinct author communities through network analysis.

Interestingly, the challenge in predicting quote popularity (demonstrated by a negative R-squared score) suggests that virality depends on complex factors beyond textual characteristics alone, including timing and network effects.

This project showcases the application of both Python and R in data science, demonstrating proficiency in:

- Advanced NLP techniques
- Network analysis
- Statistical modeling
- Data visualization
- Machine learning
- Causal inference

Technologies used: Python, R, BERT, NetworkX, scikit-learn, pandas, matplotlib

This project can also be found on my [personal website](https://jormur.github.io/).
