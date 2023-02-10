#!/usr/bin/env python
# coding: utf-8


import argparse
import sys
import re
import requests
from bs4 import BeautifulSoup
import config
from atcodertools.client.models.problem_content import remove_non_jp_characters


LOGIN_URL = "https://atcoder.jp/login"

def main():
    """
    atcoderの問題のスクレイピング用スクリプト
    pipenv run python scrape.py https://atcoder.jp/contests/abc145/tasks/abc145_c
    """

    # セッション開始
    session = requests.session()

    # csrf_token取得
    resp = session.get(LOGIN_URL)
    soup = BeautifulSoup(resp.text, 'lxml')
    csrf_token = soup.find(attrs={'name': 'csrf_token'}).get('value')

    # パラメータセット
    login_info = {
        "csrf_token": csrf_token,
        "username": config.USERNAME,
        "password": config.PASSWORD
    }

    result = session.post(LOGIN_URL, data=login_info)
    result.raise_for_status()
    if result.status_code != 200:
        print("login failed...", file=sys.stderr)
        sys.exit(1)

    args = parse_args()
    # /html/body/div[3]/div/div[1]/div[2]/div/h3[1]
    # /html/body/div[3]/div/div[1]/div[2]/div/section[1]

    # for tag in soup.select('#task-statement')[0]:
    #     print(tag)
    #     if i % 2 == 1:
    #         # arr.append(tag)
    #         print("")
    #     else:
    #         # arr.append(tag)
    #         # pairs.append(copy.deepcopy(arr))
    #         arr = []

    output_one_problem_for_scrapbox(args.url, session)

def get_problem_data(url, session):
    resp = session.get(url)
    soup = BeautifulSoup(resp.text, "html.parser")
    for elem in soup.findAll("span", {"class": "lang-en"}):
        elem.extract()
    problem = soup.select('#task-statement')
    headers = problem[0].find_all("h3")
    secs = problem[0].find_all("section")
    # task_statement = []
    restriction_tags = []
    input_tags = []
    output_tags = []
    input_format_tag = None
    output_format_tag = None
    for tag in secs:
        h3tag = tag.find('h3')
        if h3tag is None:
            continue
        # Some problems have strange characters in h3 tags which should be
        # removed
        section_title = remove_non_jp_characters(tag.find('h3').get_text())
        # if section_title.startswith("問題文"):
        #     task_statement = tag.select('h3 ~ p')
        if section_title.startswith("制約"):
            restriction_tags = tag.find("ul")
        elif section_title.startswith("入力例"):
            input_tags.append(tag.find('pre'))
        elif section_title == "入力":
            input_format_tag = tag
        elif section_title.startswith("出力例"):
            output_tags.append(tag.find('pre'))
        elif section_title == "出力":
            output_format_tag = tag

    for sec in secs[3:]:
        sec.find("h3").extract()

    data = {
        "URL": resp.url,
        "title": soup.title.text,
        "contest_name": soup.select(".contest-title")[0].text,
        "contest_id": resp.url.split("/")[4],
        "problem_str": soup.title.text.split(" - ")[0],
        "problem_name": soup.title.text.split(" - ")[1],
        "restriction": restriction_tags,
        "problem": secs[0],
        "input": input_format_tag,
        "output": output_format_tag,
        "ios": [[hr.text, sec] for hr, sec in zip(headers[4:], secs[4:])]
    }
    # print(data)
    return data


def replace_formula(data):
    for x in data.find_all(re.compile("var|code")):
        x.insert_before("[$ " + x.get_text())
        x.insert_after(" ]")
        x.decompose()


def replace_newline(data):
    # return re.sub("\n$", "", re.sub("^\n", "", re.sub("(\r\n)+", "\n", data.text)))
    return re.sub("^\n", "", re.sub("(\r\n)+", "\n", data.text))


def format_ios(data):
    # return re.sub("\n$", "", re.sub("^\n", "", re.sub("(\r\n)+", "\n", data.text)))
    # "code:memo\n" +
    #  + "\n"
    # 改行を1つ\nにする
    # →先頭行に空白をつける
    # for tag in data:
    #     print("tag " + str(i + 1))
    #     i = i + 1
    # print("Name: " + str(tag.name or "") + " " + "Text: " + str(tag.text or ""))
    # print(list(filter(lambda x: x != None or x.text != "\n", tag)))
    # tag.text !
    tags = list(filter(lambda x: x is not None and x.text != "\n",
                       data))
    # txt = " " + "\n ".join(re.sub("\n\n+", "\n", re.sub("(\r\n)+", "\n", data.text)).strip().split("\n"))

    def func(_tag):
        if _tag.name == "pre":
            txt = "\n ".join(re.sub("\n\n+",
                                    "\n",
                                    re.sub("(\r\n)+", "\n", _tag.text))
                             .strip()
                             .split("\n"))
            return "code:memo\n" + " " + txt

        return _tag.text

    txt = "\n".join([func(tag) for tag in tags])
    return txt


def output_one_problem_for_scrapbox(url, session):
    data = get_problem_data(url, session)
    replace_formula(data["problem"])
    replace_formula(data["restriction"])
    replace_formula(data["input"])
    replace_formula(data["output"])

    print(data["contest_id"].upper() + " " + data["title"])
    print("[* 問題文]")
    print(replace_newline(data["problem"]).strip())
    print("[* 制約]")
    print(replace_newline(data["restriction"]).strip())
    print("[* 入力]")
    print(replace_newline(data["input"]).strip())
    print("[* 出力]")
    print(replace_newline(data["output"]).strip())
    print("\n")
    for item in data["ios"]:
        print("[* " + item[0] + "]")
        replace_formula(item[1])
        # print(item[1])
        print(format_ios(item[1]) + "\n")

    problem_link = data["URL"] + " " + (data["title"] +
                                        " - " +
                                        data["contest_name"])
    print("[" + problem_link + "]")
    print("\n\n\n")
    tags = ["#AtCoder", "#" + data["contest_id"]]
    print(" ".join(tags))


def parse_args():
    desc = 'AtCoderの問題をScrapboxに転記する用のスクレイピングスクリプト'
    parser = argparse.ArgumentParser(description=desc)
    parser.add_argument('url', help='AtCoderのURL')
    return parser.parse_args()


if __name__ == '__main__':
    main()
