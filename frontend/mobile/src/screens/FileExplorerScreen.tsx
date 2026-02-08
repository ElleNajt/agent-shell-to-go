import React, { useState, useEffect } from 'react';
import {
  View,
  Text,
  StyleSheet,
  FlatList,
  TouchableOpacity,
  ActivityIndicator,
  Image,
  ScrollView,
} from 'react-native';
import { api, FileEntry, FileContent } from '../api/client';

interface FileExplorerScreenProps {
  initialPath: string;
  onClose: () => void;
}

export function FileExplorerScreen({ initialPath, onClose }: FileExplorerScreenProps) {
  const [currentPath, setCurrentPath] = useState(initialPath);
  const [files, setFiles] = useState<FileEntry[]>([]);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState<string | null>(null);
  const [viewingFile, setViewingFile] = useState<FileContent | null>(null);
  const [pathHistory, setPathHistory] = useState<string[]>([initialPath]);

  const loadDirectory = async (path: string) => {
    setLoading(true);
    setError(null);
    try {
      const result = await api.listFiles(path);
      // Sort: directories first, then alphabetically
      const sorted = result.files.sort((a, b) => {
        if (a.is_dir && !b.is_dir) return -1;
        if (!a.is_dir && b.is_dir) return 1;
        return a.name.localeCompare(b.name);
      });
      setFiles(sorted);
      setCurrentPath(result.path);
    } catch (e: any) {
      setError(e.message || 'Failed to load directory');
    } finally {
      setLoading(false);
    }
  };

  const openFile = async (path: string) => {
    setLoading(true);
    setError(null);
    try {
      const content = await api.readFile(path);
      setViewingFile(content);
    } catch (e: any) {
      setError(e.message || 'Failed to open file');
    } finally {
      setLoading(false);
    }
  };

  useEffect(() => {
    loadDirectory(initialPath);
  }, [initialPath]);

  const handleItemPress = (item: FileEntry) => {
    const newPath = `${currentPath}/${item.name}`;
    if (item.is_dir) {
      setPathHistory([...pathHistory, newPath]);
      loadDirectory(newPath);
    } else {
      openFile(newPath);
    }
  };

  const handleBack = () => {
    if (viewingFile) {
      setViewingFile(null);
      return;
    }
    if (pathHistory.length > 1) {
      const newHistory = pathHistory.slice(0, -1);
      setPathHistory(newHistory);
      loadDirectory(newHistory[newHistory.length - 1]);
    }
  };

  const formatSize = (bytes: number): string => {
    if (bytes < 1024) return `${bytes} B`;
    if (bytes < 1024 * 1024) return `${(bytes / 1024).toFixed(1)} KB`;
    return `${(bytes / (1024 * 1024)).toFixed(1)} MB`;
  };

  const getFileIcon = (name: string, isDir: boolean): string => {
    if (isDir) return '📁';
    const ext = name.split('.').pop()?.toLowerCase();
    switch (ext) {
      case 'png':
      case 'jpg':
      case 'jpeg':
      case 'gif':
      case 'webp':
      case 'svg':
        return '🖼️';
      case 'md':
      case 'txt':
      case 'org':
        return '📝';
      case 'py':
        return '🐍';
      case 'js':
      case 'ts':
      case 'tsx':
      case 'jsx':
        return '📜';
      case 'json':
      case 'yaml':
      case 'yml':
      case 'toml':
        return '⚙️';
      case 'el':
        return '🟣';
      case 'go':
        return '🔵';
      case 'rs':
        return '🦀';
      default:
        return '📄';
    }
  };

  const displayPath = currentPath.replace(/^\/Users\/[^/]+/, '~');

  const renderFileItem = ({ item }: { item: FileEntry }) => (
    <TouchableOpacity style={styles.fileItem} onPress={() => handleItemPress(item)}>
      <Text style={styles.fileIcon}>{getFileIcon(item.name, item.is_dir)}</Text>
      <View style={styles.fileInfo}>
        <Text style={styles.fileName} numberOfLines={1}>{item.name}</Text>
        {!item.is_dir && (
          <Text style={styles.fileSize}>{formatSize(item.size)}</Text>
        )}
      </View>
      {item.is_dir && <Text style={styles.chevron}>›</Text>}
    </TouchableOpacity>
  );

  // File viewer
  if (viewingFile) {
    return (
      <View style={styles.container}>
        <View style={styles.header}>
          <TouchableOpacity onPress={handleBack} style={styles.backButton}>
            <Text style={styles.backText}>‹ Back</Text>
          </TouchableOpacity>
          <Text style={styles.headerTitle} numberOfLines={1}>
            {viewingFile.path.split('/').pop()}
          </Text>
          <TouchableOpacity onPress={onClose} style={styles.closeButton}>
            <Text style={styles.closeText}>Done</Text>
          </TouchableOpacity>
        </View>

        {viewingFile.type === 'image' ? (
          <View style={styles.imageContainer}>
            <Image
              source={{ uri: `data:${viewingFile.mime};base64,${viewingFile.content}` }}
              style={styles.image}
              resizeMode="contain"
            />
          </View>
        ) : (
          <ScrollView style={styles.textContainer} horizontal>
            <ScrollView>
              <Text style={styles.textContent} selectable>
                {viewingFile.content}
              </Text>
            </ScrollView>
          </ScrollView>
        )}
      </View>
    );
  }

  return (
    <View style={styles.container}>
      <View style={styles.header}>
        <TouchableOpacity 
          onPress={handleBack} 
          style={styles.backButton}
          disabled={pathHistory.length <= 1}
        >
          <Text style={[
            styles.backText, 
            pathHistory.length <= 1 && styles.backTextDisabled
          ]}>
            ‹ Back
          </Text>
        </TouchableOpacity>
        <Text style={styles.headerTitle} numberOfLines={1}>
          Files
        </Text>
        <TouchableOpacity onPress={onClose} style={styles.closeButton}>
          <Text style={styles.closeText}>Done</Text>
        </TouchableOpacity>
      </View>

      <View style={styles.pathBar}>
        <Text style={styles.pathText} numberOfLines={1}>{displayPath}</Text>
      </View>

      {loading ? (
        <View style={styles.centered}>
          <ActivityIndicator size="large" color="#007AFF" />
        </View>
      ) : error ? (
        <View style={styles.centered}>
          <Text style={styles.errorText}>{error}</Text>
          <TouchableOpacity 
            onPress={() => loadDirectory(currentPath)} 
            style={styles.retryButton}
          >
            <Text style={styles.retryText}>Retry</Text>
          </TouchableOpacity>
        </View>
      ) : files.length === 0 ? (
        <View style={styles.centered}>
          <Text style={styles.emptyText}>Empty directory</Text>
        </View>
      ) : (
        <FlatList
          data={files}
          keyExtractor={(item) => item.name}
          renderItem={renderFileItem}
          style={styles.list}
        />
      )}
    </View>
  );
}

const styles = StyleSheet.create({
  container: {
    flex: 1,
    backgroundColor: '#121212',
  },
  header: {
    flexDirection: 'row',
    alignItems: 'center',
    justifyContent: 'space-between',
    padding: 12,
    borderBottomWidth: 1,
    borderBottomColor: '#333',
  },
  backButton: {
    padding: 8,
    minWidth: 60,
  },
  backText: {
    color: '#007AFF',
    fontSize: 16,
  },
  backTextDisabled: {
    color: '#444',
  },
  headerTitle: {
    color: '#FFFFFF',
    fontSize: 17,
    fontWeight: '600',
    flex: 1,
    textAlign: 'center',
  },
  closeButton: {
    padding: 8,
    minWidth: 60,
    alignItems: 'flex-end',
  },
  closeText: {
    color: '#007AFF',
    fontSize: 16,
  },
  pathBar: {
    padding: 10,
    backgroundColor: '#1E1E1E',
    borderBottomWidth: 1,
    borderBottomColor: '#333',
  },
  pathText: {
    color: '#888',
    fontSize: 12,
    fontFamily: 'monospace',
  },
  list: {
    flex: 1,
  },
  fileItem: {
    flexDirection: 'row',
    alignItems: 'center',
    padding: 14,
    borderBottomWidth: 1,
    borderBottomColor: '#2D2D2D',
  },
  fileIcon: {
    fontSize: 20,
    marginRight: 12,
  },
  fileInfo: {
    flex: 1,
  },
  fileName: {
    color: '#FFFFFF',
    fontSize: 15,
  },
  fileSize: {
    color: '#666',
    fontSize: 12,
    marginTop: 2,
  },
  chevron: {
    color: '#444',
    fontSize: 20,
  },
  centered: {
    flex: 1,
    justifyContent: 'center',
    alignItems: 'center',
  },
  errorText: {
    color: '#FF3B30',
    fontSize: 14,
    textAlign: 'center',
    marginBottom: 12,
  },
  retryButton: {
    padding: 12,
  },
  retryText: {
    color: '#007AFF',
    fontSize: 16,
  },
  emptyText: {
    color: '#666',
    fontSize: 16,
    fontStyle: 'italic',
  },
  imageContainer: {
    flex: 1,
    justifyContent: 'center',
    alignItems: 'center',
    padding: 16,
  },
  image: {
    width: '100%',
    height: '100%',
  },
  textContainer: {
    flex: 1,
    padding: 12,
  },
  textContent: {
    color: '#E0E0E0',
    fontFamily: 'monospace',
    fontSize: 12,
    lineHeight: 18,
  },
});
