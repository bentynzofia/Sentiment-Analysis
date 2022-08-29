from bs4 import BeautifulSoup
import requests
import re
from webbot import Browser

root = 'https://www.drugs.com/comments'
website = f'{root}/oxycodone/?sort_reviews=most_recent'

web = Browser()
web.go_to('drugs.com')
web.click('Accept')
web.click('Sign in', tag='a')
web.type('zofia.bentyn@gmail.com', into='username')
web.type('LEF8oz9sr', into='password')
web.click('Sign In', tag='input')

year = re.compile(r'.* 20\d{2}.*')

comments = []
dates = []
dates_temp = []

#51 podstron
for i in range(1, 44):
    web.go_to(f'{website}&page={i}')
    web.click('Accept')
    web.click('Sign in', tag='a')
    web.type('zofia.bentyn@gmail.com', into='username')
    web.type('LEF8oz9sr', into='password')
    web.click('Sign In', tag='input')

    content = web.get_page_source()
    soup = BeautifulSoup(content, 'lxml')

    comment_div = soup.find_all('div', class_='ddc-comment ddc-box ddc-mgb-2')
    date_div = soup.find_all('div', class_='ddc-comment-header')

    for container in comment_div:
        comment = container.p.text.strip()
        comments.append(comment)

    for container in date_div:
        date = container.find_all('span')
        for d in date:
            if year.match(d.text.strip()):
                dates.append(d.text.strip()[-4:])


#comments_years = [str(x) + str(y) for x,y in zip(comments, dates)]

for i in range(len(comments)):
    with open('oxycodone_' + dates[i] + '.txt', 'a', encoding='utf-8') as file:
        file.write(comments[i] + dates[i])

print(comments)
print(dates)









