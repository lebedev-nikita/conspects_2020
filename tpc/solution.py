from typing import List, Tuple
import re

RU_MONTH_VALUES = {
    'января': '01',
    'февраля': '02',
    'марта': '03',
    'апреля': '04',
    'мая': '05',
    'июня': '06',
    'июля': '07',
    'августа': '08',
    'сентября': '09',
    'октября': '10',
    'ноября': '11',
    'декабря': '12',
}


class Solution:

    def __init__(self):
        pass

    def train(self, train: List[Tuple[str, dict]]) -> None:
        # fit your models here
        pass

    def getType(self, docString):
        reType = r'федеральный закон|постановление|приказ|распоряжение|закон|указ'
        ret = re.search(reType, docString, re.I)
        ret = ret.group(0) if ret else 'NULL'
        return ret

    def getDate(self, docString):
        reDate = r'\s(\d\d?)[ \.]+(.*?)[ \.]+(\d\d\d\d)'
        ret = re.search(reDate, docString, re.I)
        if not ret:
            return 'NULL'
        elif RU_MONTH_VALUES.get(ret.group(2)):
            return '.'.join([('0' + ret.group(1))[-2:], RU_MONTH_VALUES[ret.group(2)], ret.group(3)])
        else:
            return '.'.join([('0' + ret.group(1))[-2:], ret.group(2), ret.group(3)])

    def getAuthority(self, docString):
        # TODO
        reAuthority = r'Принят.? (.*)\s'
        ret = re.search(reAuthority, docString, re.I)
        ret = ret.group(0) if ret else 'NULL'
        return ret

    def getNumber(self, docString):
        reNumber = r'[№|N][ _]*(\d+(?:-?.*?))(?:\s|$|,)'
        ret = re.search(reNumber, docString, re.I)
        ret = ret.group(1) if ret else 'NULL'
        return ret

    def getName(self, docString):
        reName = r'\n("?Об? .*?)\n'
        ret = re.search(reName, docString, re.I)
        ret = ret.group(1) if ret else 'NULL'
        return ret

    def predict(self, test: List[str]) -> List[dict]:
        # Do some predictions here and return results
        # Let's return empty result in proper format
        results = []
        for docString in test:
            prediction = {"type": self.getType(docString),
                          "date": self.getDate(docString),
                          "number": self.getNumber(docString),
                          "authority": self.getAuthority(docString),
                          "name": self.getName(docString)}
            results.append(prediction)
        return results
