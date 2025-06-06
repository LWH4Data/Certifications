import pandas as pd

s = pd.Series(['abc.123', 'def.456'])
print(s.str.replace('.', '', regex=True))

# 결과: ['abc123', 'def456']  # .은 모든 문자라서, 모든 문자가 제거됨!

a = pd.Series(['abc.123', 'def.456'])
print(a.str.replace('.', '', regex=False))

# 결과: ['abc123', 'def456']  # . 문자(온점)만 제거됨