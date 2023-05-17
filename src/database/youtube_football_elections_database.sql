-- CREATE DATABASE youtube_football_elections;

BEGIN TRANSACTION;


CREATE SCHEMA contents;

CREATE TABLE contents.comments(
    comment_id VARCHAR(200),
    video_id VARCHAR(200),
    parent_id VARCHAR(200),
    user_name VARCHAR(200),
    user_channel_url TEXT,
    comment_text_display TEXT,
    comment_text_original TEXT,
    can_rate BOOLEAN,
    viewer_rating INT,
    like_count INT,
    published_at TIMESTAMP,
    updated_at TIMESTAMP,
    moderation_status VARCHAR(50),
    user_id VARCHAR(200),
    toxicity_score FLOAT
);

COMMIT TRANSACTION;
-- ROLLBACK TRANSACTION;