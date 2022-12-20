from TwitterAPI import TwitterAPI, TwitterRequestError, TwitterConnectionError, HydrateType
import pandas as pd
import time
import sys
import os

consumer_key = 'mpcIe20x0nbqvlH5qCMNHnoaj'
consumer_secret = 'vsuY3UZrImDgclLl0p760K0flkwjsYbRekdIxI7Wu1rXEvCATr'
bearer_token = 'AAAAAAAAAAAAAAAAAAAAAJZ8jwEAAAAAbV5QD%2BNvLlVLOG8xdZZ82UQcaF0%3DxKNJ6J8nFTECSsXFFgPcMBMtua7jc6MUet6kLBH4XQieAxgwHj'
access_token_key = ''
access_token_secret = ''
folder = "/media/gabett/Volume/data-repository/panconesi-football-elections/elections/test/"
filename = folder + "test_ids.csv"
output_folder = folder
log_filename = "../logs/twitter_news_log.txt"

def save_df_updated(df_supp, data_type, usr, data_folder):
    string_savefile = data_folder+'Twitter_'+data_type+'_'+str(usr)+'.csv'
    df_supp.to_csv(string_savefile, index=False)

def adjust_columns_append_df(df_old, df_supp):
    add_columns_to_old = list(set(df_supp.columns)-set(df_old.columns))
    for column in add_columns_to_old:
        df_old[column] = ['' for i in range(len(df_old))]
    add_columns_to_supp = list(set(df_old.columns)-set(df_supp.columns))
    for column in add_columns_to_supp:
        df_supp[column] = ['' for i in range(len(df_supp))]
    return pd.concat([df_old,df_supp], ignore_index=True)

def query(api, conversation_id, start_t, end_t, max_results, string_expansions, string_media_fields, string_place_fields,
          string_poll_fields, string_tweet_fields, string_user_fields, next_token, include_next_token):
    
    try:
        start = start_t+'T00:00:00Z'
        end = end_t+ 'T00:00:00Z'
        if include_next_token == False:
            r = api.request('tweets/search/all',
                            {
                                'query':f'conversation_id:{conversation_id}',
                                'start_time': start,
                                'end_time': end,
                                'max_results': max_results,
                                'expansions': string_expansions,
                                'media.fields': string_media_fields,
                                'place.fields': string_place_fields,
                                'poll.fields': string_poll_fields,
                                'tweet.fields': string_tweet_fields,
                                'user.fields': string_user_fields
                            },
                            hydrate_type=HydrateType.APPEND)

        elif include_next_token == True:
            r = api.request('tweets/search/all',
                            {
                                'query':f'conversation_id:{conversation_id}',
                                'start_time': start,
                                'end_time': end,
                                'max_results': max_results,
                                'expansions': string_expansions,
                                'media.fields': string_media_fields,
                                'place.fields': string_place_fields,
                                'poll.fields': string_poll_fields,
                                'tweet.fields': string_tweet_fields,
                                'user.fields': string_user_fields,
                                'next_token': next_token
                            },
                            hydrate_type=HydrateType.APPEND)
        else:
            print('Variable "include_next_token" should be either "True" or "False"')
            return -1

        return r

    except TwitterRequestError as e:
        print(e.status_code)
        for msg in iter(e):
            print(msg)
        return -1

    except TwitterConnectionError as e:
        print(e)
        return -1

    except Exception as e:
        print(e)
        return -1

    except:
        print(r.json().keys())
        for key in r.json(): print(r.json()[key])
        return -1

def page_timeline(conversation_id,start_date,end_date,data_folder):

    api = TwitterAPI(consumer_key, consumer_secret, access_token_key, access_token_secret, auth_type='oAuth2', api_version='2')
    api.REST_TIMEOUT=20
    api.CONNECTION_TIMEOUT=20


    expansions = ['attachments.poll_ids', 'attachments.media_keys', 'author_id', 'entities.mentions.username',
              'geo.place_id', 'in_reply_to_user_id', 'referenced_tweets.id', 'referenced_tweets.id.author_id']
    string_expansions = ','.join(expansions[:])
    media_fields= ['duration_ms', 'height', 'media_key', 'preview_image_url', 'type', 'url', 'width', 'public_metrics']
    string_media_fields = ','.join(media_fields[:])
    place_fields = ['contained_within', 'country', 'country_code', 'full_name', 'geo', 'id', 'name', 'place_type']
    string_place_fields = ','.join(place_fields[:])
    poll_fields = ['duration_minutes', 'end_datetime', 'id', 'options', 'voting_status']
    string_poll_fields = ','.join(poll_fields[:])
    tweet_fields = ['attachments', 'author_id',
                    'conversation_id', 'created_at', 'entities',
                    'geo', 'id', 'in_reply_to_user_id', 'lang', 'public_metrics', 'possibly_sensitive', 'referenced_tweets',
                    'reply_settings', 'source', 'text', 'withheld']
    string_tweet_fields = ','.join(tweet_fields[:])
    user_fields = ['created_at', 'description', 'entities', 'id', 'location', 'name', 'pinned_tweet_id', 'profile_image_url',
                'protected', 'public_metrics', 'url', 'username', 'verified', 'withheld']
    string_user_fields = ','.join(user_fields[:])  


    include_next_token=False
    next_token="start"
    df = {}
    sys.stdout.writelines("downloading tweet with conversation_id: "+str(conversation_id)+"\n")
    sys.stdout.flush()

    trial_count=0
    while next_token is not None:
        time.sleep(1)
        r = query(api, str(conversation_id), start_date, end_date, 500,
                string_expansions, string_media_fields, string_place_fields,
                string_poll_fields, string_tweet_fields, string_user_fields,
                next_token=next_token, include_next_token=include_next_token)
        if r!=-1:
            if r.status_code == 200:
                try:
                    trial_count = 0
                    if 'errors' in r.json().keys() & 'data' not in r.json().keys():
                        sys.stdout.writelines("error in getting data\n")
                        sys.stdout.writelines(r.json()['errors'][0]['detail']+"\n")
                        sys.stdout.flush()
                        next_token = None
                        include_next_token = False
                    else:
                        if 'next_token' not in r.json()['meta']:
                            next_token = None
                            include_next_token=False
                        else:
                            next_token = r.json()['meta']['next_token']
                            print("token updated: "+next_token)
                            include_next_token=True
                    try:
                        if r.get_quota()['remaining'] is None:
                            sys.stdout.writelines("quota finished! go to sleep for a while...\n")
                            sys.stdout.flush()
                            time.sleep(600)
                    except:
                        sys.stdout.writelines("error: unable to get quota\n")
                        sys.stdout.flush()
                except:
                    sys.stdout.writelines("error getting next token\n")
                    sys.stdout.flush()

                try:
                    df_supp = pd.DataFrame(r.json()['data'], dtype=str)
                    df.setdefault('data', pd.DataFrame({}))
                    df['data'] = adjust_columns_append_df(df['data'], df_supp)

                    for key in r.json()['includes']:
                        df.setdefault(key, pd.DataFrame({}))
                        df_supp = pd.DataFrame(r.json()['includes'][key], dtype=str)
 
                        df[key] = adjust_columns_append_df(df[key], df_supp)

                except Exception as e:
                    sys.stdout.writelines("unable to append reults\n")
                    sys.stdout.flush()
            elif r.status_code == 503:
                sys.stdout.writelines("error " + str(r.status_code) + '\n')
                sys.stdout.flush()
                trial_count=trial_count+1
                return
            else:
                try:
                    if r.status_code == 429:
                        sys.stdout.writelines("quota finished! go to sleep for a while...\n")
                        sys.stdout.flush()
                        time.sleep(600)
                    else:
                        sys.stdout.writelines("error "+str(r.status_code)+'\n')
                        sys.stdout.flush()
                        return
                except:
                    sys.stdout.writelines("network error/no readable answer" '\n')
        if trial_count>300:
            next_token = None
            include_next_token = False
            trial_count=0
            with open('failed.txt', 'a+') as failed_file:
                failed_file.write(str(conversation_id)+'\n')
            sys.stdout.writelines("gave up with " + str(conversation_id) + "\n")
            sys.stdout.flush()

    sys.stdout.writelines("saving results for "+str(conversation_id)+"\n")
    sys.stdout.flush()
    if len(df)>0:
        save_df_updated(df['data'], 'comments_', conversation_id, data_folder)
        return df

if __name__ == "__main__":

    df = pd.read_csv(filename)

    i = 0    

    # if(os.path.isfile(log_filename)):
    #     with open(log_filename, "r") as f:
    #         i = int(f.read())
    
    for conversation_id in df["conversation_id"][i:]:
        conversation_id = str(conversation_id)
        page_timeline(conversation_id, "2018-01-01", "2022-12-10", output_folder)

        with open(log_filename, "w") as f:
            f.write(str(i))

        i = i + 1