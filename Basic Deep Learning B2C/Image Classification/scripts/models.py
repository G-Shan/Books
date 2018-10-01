import tensorflow as tf

def cnn(x_dict):
    """ Implementation of a convolutional neural network."""

    x = tf.convert_to_tensor(list(x_dict.values()))
    x = tf.cast(x, dtype=tf.float16)
    # Obtain and reshape the data.
    x = tf.reshape(x, (-1,60,60,3))
    # First Layer (conv+maxpool)
    conv1 = tf.layers.conv2d(inputs=x, filters=32,
        kernel_size=[5,5], padding="same", activation=tf.nn.relu)
    pool1 = tf.layers.average_pooling2d(inputs=conv1, pool_size=[2,2], strides=2)
    # Second Layer (conv+maxpool)
    conv2 = tf.layers.conv2d(inputs=pool1, filters=64,
        kernel_size=[5,5], padding="same", activation=tf.nn.relu)
    pool2 = tf.layers.average_pooling2d(inputs=conv2, pool_size=[2,2], strides=2)
    # Reshape pool2 into two dimensions.
    conv3 = tf.layers.conv2d(inputs=pool2, filters=128,
        kernel_size=[2,2], padding='same', activation=tf.nn.relu) 
    pool3 = tf.layers.average_pooling2d(inputs=conv3, pool_size=[2,2], strides=2)
    pool3_flat = tf.reshape(pool3, [-1,7*7*128])
    # FC Layer.
    dense = tf.layers.dense(inputs=pool3_flat, units=512, activation=tf.nn.relu)
    # Dropout regularization.
    dropout = tf.layers.dropout(inputs=dense, rate=0.8)
    #Logits layer.
    output_layer = tf.layers.dense(inputs=dropout, units=7)
    return output_layer
