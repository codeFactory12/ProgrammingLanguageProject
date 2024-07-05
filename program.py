import pandas as pd

data = pd.read_csv("data.csv")

data = data[["name", "age", "salary", "department"]]

data = data[data["age"]> 30]
data = data[["name"]]

data.to_csv("names_filtered.csv", index=False)

result = 0
flag = True
message = "Process completed"

def calculate(a, b):
    sum = a + b
    return sum

def processData(data):
    datafunc = pd.read_csv("data.csv")
    datafunc = datafunc[datafunc["salary"]> 50000]
    datafunc = datafunc.groupby(["salary"]).sum()
    datafunc.to_csv("filtered_salary.csv", index=False)

processData(data)
result = calculate(10, 20)

while flag:
    result = result + 1
    if result > 10:
        flag = False


if result == 11:
    print("The result is 11")
else:
    print("The result is not 11")


match result:
    case 11:
        print("The result matches 11")
    case 20:
        print("The result matches 20")
    case _:
        print("The result does not match any case")

print(message)
