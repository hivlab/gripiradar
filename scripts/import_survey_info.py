import csv
import json
from pathlib import Path


def parse_survey_info(survey_type: str) -> None:
    json_path = Path(f"gripiradar-ut-ee/{survey_type}/survey_info.json")
    out_path = Path(f"data/{survey_type}_survey_info.csv")

    with json_path.open(encoding="utf-8") as f:
        data = json.load(f)

    versions = data["versions"][0]
    rows = []
    for q in versions["questions"]:
        responses = q.get("responses") or []
        options = responses[0].get("options", []) if responses else []
        if not options:
            rows.append([q["key"], q["title"], q["questionType"], "", ""])
            continue
        for o in options:
            rows.append([
                q["key"],
                q["title"],
                q["questionType"],
                o.get("key", ""),
                o.get("label", ""),
            ])

    with out_path.open("w", newline="", encoding="utf-8") as f:
        writer = csv.writer(f, lineterminator="\n")
        writer.writerow(["question", "title", "questionType", "key", "label"])
        writer.writerows(rows)


if __name__ == "__main__":
    for survey_type in ("intake", "vaccination", "weekly"):
        print(f"=== {survey_type} ===")
        parse_survey_info(survey_type)
        print(f"  wrote data/{survey_type}_survey_info.csv")
