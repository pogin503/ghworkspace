#!/usr/bin/env python
# coding: utf-8


"""
atcoderの問題のスクレイピング用スクリプト
pipenv run start
"""

import sys
import re
import requests
import copy
from bs4 import BeautifulSoup
import config

LOGIN_URL = "https://atcoder.jp/login"

url = "http://arc001.contest.atcoder.jp/tasks/arc001_1"

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

    output_problem_for_scrapbox(get_problem_data(url))

def get_problem_data(url):
    req = requests.get(url)
    soup = BeautifulSoup(req.text, "html.parser")
    problem = soup.select('#task-statement')
    hrs = problem[0].find_all("h3")
    secs = problem[0].find_all("section")

    data = {
        "URL": req.url,
        "title": soup.title.text,
        "contest_name": soup.select(".contest-title")[0].text,
        "contest_id": req.url.split("/")[4],
        "problem_str": soup.title.text.split(" - ")[0],
        "problem_name": soup.title.text.split(" - ")[1],
        "problem": secs[0],
        "input": secs[1],
        "output": secs[2],
        "ios": [[hr.text, sec] for hr, sec in zip(hrs[3:], secs[3:])]
    }
    return data

def replace_formula(data):
    for x in data.find_all(re.compile("var|code")):
        x.insert_before("[$ " + x.get_text())
        x.insert_after(" ]")
        x.decompose()

def replace_newline(data):
    return re.sub("\n$", "", re.sub("^\n", "", re.sub("(\r\n)+", "\n ", data.text)))

def output_problem_for_scrapbox(data):
    replace_formula(data["problem"])
    replace_formula(data["input"])
    replace_formula(data["output"])

    print(data["contest_id"].upper() + " " + data["title"])
    print("[* 問題文]")
    print(replace_newline(data["problem"]).strip())
    print("[* 入力]")
    print(replace_newline(data["input"]).strip())
    print("[* 出力]")
    print(replace_newline(data["output"]).strip())
    for item in data["ios"]:
        print("[* " + item[0] + "]")
        replace_formula(item[1])
        # print(item[1])
        print("code:memo" + replace_newline(item[1]))

    print("[" + data["URL"] + " " +  data["title"] + " - " + data["contest_name"] +"]")
    print("\n\n\n")
    print(" ".join(["#AtCoder", "#" + data["contest_id"]]))

if __name__ == '__main__':
    main()
