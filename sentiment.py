from textblob import TextBlob
import pandas as pd
import os
from vaderSentiment.vaderSentiment import SentimentIntensityAnalyzer
def sentiment_cal(diary, mood):
    sentiment_scores= SentimentIntensityAnalyzer().polarity_scores(diary)
    neg, neu, pos, compound = sentiment_scores['neg'], sentiment_scores['neu'], sentiment_scores['pos'], sentiment_scores['compound']
    mood_score = (compound + 1) * 4.5 + 1 - 5
    if (neg > 0.5):
        mood_score -= neg * 2
    elif(pos > 0.5):
        mood_score*= pos*2
    else:
        mood_score *= (0.5 + neu)
    mood_output = mood * 0.65 + mood_score *0.35
    return mood_output
# da = df['Diary'][0]
# mood = df['Mood'][0]
# mood
# sentiment_cal(da, mood)
