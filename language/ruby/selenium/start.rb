require "selenium-webdriver"

# Firefox用のドライバを使う
driver = Selenium::WebDriver.for :firefox

# Googleにアクセス
driver.navigate.to "http://google.com"

# `q`というnameを持つ要素を取得
element = driver.find_element(:name, 'q')

# `Hello WebDriver!`という文字を、上記で取得したinput要素に入力
element.send_keys "Hello WebDriver!"

# submitを実行する（つまり検索する）
element.submit

# 表示されたページのタイトルをコンソールに出力
puts driver.title

# テストを終了する（ブラウザを終了させる）
driver.quit
