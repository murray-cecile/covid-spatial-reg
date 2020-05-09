#==============================================================================#
# COLLECT COVID DATA FROM IL DEPT OF PUBLIC HEALTH WEBSITE
#
# Cecile Murray
#==============================================================================#

import time
import requests
from bs4 import BeautifulSoup

ILDPH_SITE = "https://www.dph.illinois.gov/covid19/covid19-statistics"


def get_site_data():
    '''
    Takes: nothing
    Returns: ILPDH site as a BS4 object
    '''

    
    r = requests.get(ILDPH_SITE)

    if r.raise_for_status():
        print(r.status_code)

    else:
        return BeautifulSoup(r.text)


if __name__ == "__main__":
    

    s = get_site_data()