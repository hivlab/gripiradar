import pandas as pd
import json
from pathlib import Path
import os

paths = ["intake/survey_info.json", "vaccination/survey_info.json", "weekly/survey_info.json"]

def import_json(path):
    with open(path, "r") as f:
        data = json.load(f)
    questions = data["versions"][0]["questions"]
    ids =  pd.json_normalize(questions)[["key", "title"]]
    ids = ids.rename(columns={"key":"Q"})
    df = pd.json_normalize(questions, record_path = ["responses"], meta = ["title"])
    cols = 'options'
    s = df[cols].explode()
    tmp = df.drop(columns=cols)

    opts = pd.concat([tmp, 
                    pd.json_normalize(s)
                    .set_axis(s.index).dropna()
                    .combine_first(tmp)
                    ]).drop_duplicates().sort_index(kind='stable')[["key", "title", "label"]]

    opts = opts[~opts.key.isin(["scg", "mcg", "ddg"])]
    out = ids.set_index("title").join(opts.set_index("title"))
    out.to_csv(Path("data", Path(path).stem + ".csv"), sep = ",")


for path in paths:
    import_json(path)
