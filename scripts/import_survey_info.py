import json
import pandas as pd

def parse_survey_info(type):
    json_path = f"gripiradar-ut-ee/{type}/survey_info.json"
    with open(json_path, "r", encoding="utf-8") as f:
        data = json.load(f)
    versions = data.get("versions")[0]
    questions = []
    for q in versions.get("questions"):
        options = q.get("responses")[0].get("options", [])
        keys = []
        labels = []
        for o in options:
            keys.append(o.get("key", ""))
            labels.append(o.get("label", ""))
        
        questions.append(
            {
                'question': q['key'],
                'title': q['title'],
                'questionType': q['questionType'],
                'key': keys,
                'label': labels,
                }
        )

    df = pd.DataFrame.from_dict(questions, orient='columns')
    df = df.explode(['key', 'label']).reset_index(drop=True)
    df.to_csv(f'data/{type}_survey_info.csv', index=False)

if __name__ == "__main__":
    for d in ['intake', 'vaccination', 'weekly']:
        print(f'\n=== {d} ===\nWorking on {d} survey info..\n')
        parse_survey_info(d)
        if d == 'intake':
            print('Note: # of family members in age groups was not parsed properly\nas we lack age group labels..')
        print('OMG! Success!\n')
