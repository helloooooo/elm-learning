import urllib.request, json
import random
import time 
if __name__ == '__main__':
    for i in range(100):
        url = "http://localhost:3090/datas" 
        method = "POST"
        headers = {"Content-Type" : "application/json"}

        # PythonオブジェクトをJSONに変換する
        obj = {"human":random.randint(0,1),"temparture": random.uniform(30,40), "userId" : 1} 
        json_data = json.dumps(obj).encode("utf-8")

        # httpリクエストを準備してPOST
        request = urllib.request.Request(url, data=json_data, method=method, headers=headers)
        with urllib.request.urlopen(request) as response:
            response_body = response.read().decode("utf-8")
        time.sleep(1)