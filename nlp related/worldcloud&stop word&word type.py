from wordcloud import WordCloud
import matplotlib.pyplot as plt
import nltk
from nltk.corpus import stopwords
from nltk.tokenize import word_tokenize
from wordcloud import WordCloud
import pandas as pd
stopwords=stopwords.words('english')

filename1_1 = "light phone 1-1.txt"
with open(filename1_1) as f:
    mytext1_1 = f.read()
f.close()
filename1_2 = "light phone 1-2.txt"
with open(filename1_2) as f:
    mytext1_2 = f.read()
f.close()
filename1_3 = "light phone 1-3.txt"
with open(filename1_3) as f:
    mytext1_3 = f.read()
f.close()
filename1_4 = "light phone 1-4.txt"
with open(filename1_4) as f:
    mytext1_4 = f.read()
f.close()
filename1_5 = "light phone 1-5.txt"
with open(filename1_5) as f:
    mytext1_5 = f.read()
f.close()
mytext=''
mytext+=mytext1_1
mytext+=' '
mytext+=mytext1_2
mytext+=' '
mytext+=mytext1_3
mytext+=' '
mytext+=mytext1_4
mytext+=' '
mytext+=mytext1_5
mytext+=' '

text = nltk.word_tokenize(mytext)

tagged_text = nltk.pos_tag(text)
print(tagged_text)
# 为避免标记的复杂化，可设置tagset为‘universal’
tagged_text = nltk.pos_tag(text, tagset='universal')
adj_list = ''
all_adj={}
for i in tagged_text:
    if i[0] in stopwords:
        continue
    if i[0] in ['i','able','other','''it's''','overall','uh','most']:
        continue
    if i[1] == 'ADJ':
        adj_list += i[0]
        adj_list += ' '
        all_adj[i[0]]=all_adj.setdefault(i[0],0)+1
adj_frame=pd.DataFrame.from_dict(all_adj, orient='index',columns=['count'])
adj_frame.sort_values('count',ascending=False,inplace=True)
adj_frame.to_excel('lightphone 1 word count.xlsx')
wordcloud = WordCloud().generate(adj_list)

plt.imshow(wordcloud, interpolation='bilinear')
plt.axis("off")
plt.show()
