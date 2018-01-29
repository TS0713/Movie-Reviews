################# IMDB reviews extraction ######################## Time Taking process as this program is going
# to operate the web page while extracting reviews 
############# time library in order to sleep and make it to extract for that specific page 
#### We need to install selenium for python
#### pip install selenium
#### time library to make the extraction process sleep for few seconds 
from selenium import webdriver
browser = webdriver.Chrome() # opens the chrome browser
from bs4 import BeautifulSoup as bs
#page = "http://www.imdb.com/title/tt0944947/reviews?ref_=tt_urv"
page = "http://www.imdb.com/title/tt6294822/reviews?ref_=tt_urv" # required url page where the movie reviews are residing

# Importing few exceptions to pass while extracting reviews 
from selenium.common.exceptions import NoSuchElementException 
from selenium.common.exceptions import ElementNotVisibleException
browser.get(page)
import time
reviews = []
i=1
while (i>0):
    i=i+25
    try:
        # Storing the load more button page xpath which we will be using it for click it through selenium 
        # for loading few more reviews
        button = browser.find_element_by_xpath('//*[@id="load-more-trigger"]')
        button.click()
        time.sleep(5)
        # getting page source
        ps = browser.page_source 
        # Converting into Beautiful soup object
        soup=bs(ps,"html.parser")
        # Extracting the reviews present in div html_tag having class = "text" has its attribute
        rev = soup.findAll("div",attrs={"class","text"})
        reviews.extend(rev)
    except NoSuchElementException:
        break
    except ElementNotVisibleException:
        break
        

##### If we want only few recent reviews you can either press cntrl+c to break the operation in middle but the it will store 
##### Whatever data it has extracted so far #######
len(reviews)
len(list(set(reviews)))
with open("reviews.txt","w") as rev:
    rev.write(str(reviews))
