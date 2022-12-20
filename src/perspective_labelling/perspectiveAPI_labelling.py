import warnings
warnings.simplefilter(action='ignore', category=FutureWarning)

import pandas as pd
from tqdm import tqdm
import numpy as np
from perspective import PerspectiveAPI
import time
import concurrent.futures
from typing import Iterable, List
import os

pd.options.mode.chained_assignment = None  # default='warn'

key = "AIzaSyBEhUYva5h8jkepOARvFs3rgGlfaPpb0WI"
n_call = 20

def split_in(path: str, n: int, name: str):
    df = pd.read_csv(path)
    list_df: List[pd.DataFrame] = np.array_split(df, n)
    for i in range(len(list_df)):
        list_df[i].to_csv(f"{i}_{name}.csv")


class downloader:
    """
    class for labelling with perspective api.
    just insert the api key and let it run
    """

    def __init__(
        self,
        path: str,
        file : str,
        steps_for_saving: int = 100,
        col_text: str = "text",
        col_id: str = "id",
    ) -> None:

        save_path = path + "labelled"
        if not os.path.exists(save_path):
            os.mkdir(save_path)

        if file.endswith(".csv"):
            df_path = path + file
            non_labelled_df = pd.read_csv(
                df_path,
                encoding="latin-1",
                lineterminator="\n",
            )
        elif file.endswith(".ndjson"):
            df_path = path + file
            non_labelled_df = pd.read_json(
                df_path,
                encoding="latin-1",
                lines=True,
            )
        non_labelled_df = non_labelled_df[non_labelled_df[col_text].notna()]

        if os.listdir(save_path) == []:
            max_i = 0
        else:
            max_i = (
                max([int(i.split("_")[0]) for i in os.listdir(save_path)]) + 1
            )
            self.merge(save_path)
            processed = pd.read_csv(
                f"{save_path.split('/')[1]}_scored.csv",
                encoding="latin-1",
                lineterminator="\n",
            )
            non_labelled_df = non_labelled_df[
                ~non_labelled_df[col_id].isin(processed[col_id])
            ]
            del processed

        self.perspective_api = PerspectiveAPI(key)

        saving_blocks = self.create_chunk(non_labelled_df, size=100000)

        for i in tqdm(range(len(saving_blocks))):

            log_content = "File: {0}\nChunk : {1}\n".format(file, i)
            with open("../../logs/perspectiveAPI_last_file_retrieved.txt", "w") as log_file:
                log_file.write(str(log_content))

            df = saving_blocks[i]
            chunk = self.create_chunk(df, size=n_call)

            result_df = pd.DataFrame(columns=[col_id, col_text, "created_at", "conversation_id", "author_id"])

            c = 0

            for chunk_df in tqdm(chunk):
                main_list = []
                time.sleep(0.9)
                with concurrent.futures.ThreadPoolExecutor() as executor:
                    res = executor.map(self.scoring_comments, chunk_df[col_text])
                    main_list += list(res)
                    chunk_df["toxicity_score"] = main_list
                    result_df = pd.concat([result_df, chunk_df])
                c += 1
                if c % steps_for_saving == 0:
                    result_df.to_csv(
                        f"{save_path}/{max_i+i}_{file}",
                        index=False,
                    )
            result_df.to_csv(
                f"{save_path}/{max_i+i}_{file}",
                index=False,
            )
        self.merge(save_path)

    def create_chunk(
        self, 
        comments_list: pd.DataFrame, 
        size: int
    ) -> List[pd.DataFrame]:
        return [comments_list[i : i + size] for i in range(0, len(comments_list), size)]

    def scoring_comments(self, str: str):
        try:
            result = self.perspective_api.score(str)
            return result["TOXICITY"]

        except Exception as e:
            try:
                if e.code != 400:
                    print(e)
                    result = self.perspective_api.score(str)
                    return result["TOXICITY"]
            except:
                print(e)
                return 0

    def merge(self, save_path: str):
        print("merging")
        final_df = pd.DataFrame()
        for i in os.listdir(save_path):
            tmp = pd.read_csv(
                "/".join([save_path, i]),
                encoding="latin-1",
                lineterminator="\n",
            )
            final_df = pd.concat([final_df, tmp])
        final_df.to_csv(
            f"{save_path.split('/')[1]}_scored.csv",
            index=True,
        )
        print("merged")


if __name__ == "__main__":

    path = "/media/gabett/Volume/data-repository/panconesi-football-elections/elections/original_tweets/comments_unified/"

    files = os.listdir(path)
    i = 0
    for f in files[i:]:
        if f.startswith("comments"):
            print(f)

            downloader(path, f, steps_for_saving=100)
