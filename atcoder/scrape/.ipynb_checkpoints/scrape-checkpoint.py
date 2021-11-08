#!/usr/bin/env python
# coding: utf-8


"""
atcoderの問題のスクレイピング用スクリプト
pipenv run start
"""

import sys
import requests
import copy

from bs4 import BeautifulSoup
import config

LOGIN_URL = "https://atcoder.jp/login"

def main():
    # セッション開始
    session = requests.session()

    # csrf_token取得
    r = session.get(LOGIN_URL)
    s = BeautifulSoup(r.text, 'lxml')
    csrf_token = s.find(attrs={'name': 'csrf_token'}).get('value')

    # パラメータセット
    login_info = {
        "csrf_token": csrf_token,
        "username": config.USERNAME,
        "password": config.PASSWORD
    }

    result = session.post(LOGIN_URL, data=login_info)
    result.raise_for_status()
    if result.status_code == 200:
        print("log in!")
    else:
        print("failed...")
        sys.exit("failed....")

    req = requests.get("http://arc001.contest.atcoder.jp/tasks/arc001_1")
    soup = BeautifulSoup(req.text, "html.parser")
    # /html/body/div[3]/div/div[1]/div[2]/div/h3[1]
    # /html/body/div[3]/div/div[1]/div[2]/div/section[1]
    i = 1
    arr = [];
    pairs = [];
    problem = soup.select('#task-statement')[0]

    # for tag in soup.select('#task-statement')[0]:
    #     print(tag)
    #     if i % 2 == 1:
    #         # arr.append(tag)
    #         print("")
    #     else:
    #         # arr.append(tag)
    #         # pairs.append(copy.deepcopy(arr))
    #         arr = []

    #     i += 1
    # print(pairs)
    #     prinjjjjt(tag)
    # for tag in soup.select('#task-statement')[0].select("td"):
    #     print(tag)


if __name__ == '__main__':
    main()
