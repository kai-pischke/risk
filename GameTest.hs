



instance ToJSON Country where
    toJSON c = toJSON $ show c

instance FromJSON Country where
    parseJSON (String s) = do
        let sR = readMaybe $ unpack s
        if (sR == Nothing)
            then do mempty
            else do return (fromJust sR)
    parseJSON _ = mempty

instance FromJSONKey Country
