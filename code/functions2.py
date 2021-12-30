def model_scores(X_train, y_train, X_test, y_test, model):
    
    from sklearn.metrics import confusion_matrix, roc_auc_score
    import pandas as pd
    
    ## training data scores
    train_tn, train_fp, train_fn, train_tp = confusion_matrix(y_train, model.predict(X_train)).ravel()
    train_accuracy = (train_tn+train_tp) / (train_tn+train_tp+train_fn+train_fp)
    train_specificity = train_tn / (train_tn+train_fp)
    train_recall = train_tp / (train_tp+train_fn)
    train_precision = train_tp / (train_tp+train_fp)
    train_f1 = 2 * (train_precision*train_recall)/(train_precision+train_recall)
    train_roc_auc = roc_auc_score(y_train, model.predict(X_train))

    ## testing data scores
    test_tn, test_fp, test_fn, test_tp = confusion_matrix(y_test, model.predict(X_test)).ravel()
    test_accuracy = (test_tn+test_tp) / (test_tn+test_tp+test_fn+test_fp)
    test_specificity = test_tn / (test_tn+test_fp)
    test_recall = test_tp / (test_tp+test_fn)
    test_precision = test_tp / (test_tp+test_fp)
    test_f1 = 2 * (test_precision*test_recall)/(test_precision+test_recall)
    test_roc_auc = roc_auc_score(y_test, model.predict(X_test))
    
    ## data frame
    scores_df = pd.DataFrame({
        'accuracy' : [train_accuracy, test_accuracy],
        'specificity': [train_specificity, test_specificity],
        'recall': [train_recall, test_recall],
        'precision': [train_precision, test_precision],
        'f1': [train_f1, test_f1],
        'roc_auc': [train_roc_auc, test_roc_auc]
    }, index=['train','test'] )
    
    return scores_df