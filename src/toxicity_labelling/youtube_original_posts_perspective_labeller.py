import json
import pandas as pd
import threading
from perspective import PerspectiveAPI
from tqdm import tqdm
import time
import sys

# funzione per elaborare un blocco di righe del dataframe
def process_block(df, block_start, block_end, perspective_api):
    for i in range(block_start, block_end):
        text = df.loc[i, 'title']
        try:
            toxicity_score = perspective_api.score(text)['TOXICITY']
            df.loc[i, 'toxicity_score'] = toxicity_score
        except Exception as e:
            print(str(e.code))
            df.loc[i, 'toxicity_score'] = None


if __name__ == "__main__":

    # leggi il file JSON che contiene i tipi di dati delle colonne
    with open(sys.argv[1]) as f:
        dtype = json.load(f)

    # leggi il file CSV con i commenti
    df = pd.read_csv(sys.argv[2], dtype=dtype)
    
    # inizializza la colonna toxicity_score
    df['toxicity_score'] = None

    # specifica il numero di chiamate al secondo
    calls_per_second = 100 # calls_per_second = sys.srgv[3] 

    # specifica la chiave dell'API di Google Perspective
    api_key = sys.argv[4]

    perspective_api = PerspectiveAPI(api_key)

    # specifica la dimensione dei blocchi di righe
    block_size = calls_per_second

    # specifica il numero di righe da elaborare in parallelo
    num_threads = block_size

    # elabora il dataframe a blocchi di dimensione block_size
    for block_start in tqdm(range(0, len(df), block_size)): 
        start_time = time.time()
        block_end = min(block_start + block_size, len(df))
        block_df = df.iloc[block_start:block_end]
        threads = []
        for i in range(block_start, block_end, 1):
            thread_start = i * (block_size // num_threads)
            thread_end = (i + 1) * (block_size // num_threads)
            thread = threading.Thread(target=process_block, args=(block_df, thread_start, thread_end, perspective_api))
            threads.append(thread)
        for thread in threads:
            thread.start()
        for thread in threads:
            thread.join()


        end_time = time.time()
        elapsed_time = end_time - start_time
        time.sleep(min(abs(1-elapsed_time), 1))

    output_file = f"{sys.argv[4]}.csv"
    df.to_parquet(output_file)